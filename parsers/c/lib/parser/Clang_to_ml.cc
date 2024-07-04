/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2017-2019 The MOPSA Project.                               */
/*                                                                          */
/* This program is free software: you can redistribute it and/or modify     */
/* it under the terms of the GNU Lesser General Public License as published */
/* by the Free Software Foundation, either version 3 of the License, or     */
/* (at your option) any later version.                                      */
/*                                                                          */
/* This program is distributed in the hope that it will be useful,          */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/* GNU Lesser General Public License for more details.                      */
/*                                                                          */
/* You should have received a copy of the GNU Lesser General Public License */
/* along with this program.  If not, see <http://www.gnu.org/licenses/>.    */
/*                                                                          */
/****************************************************************************/


/*
  Clang_to_ml - Parse C files with Clang into AST and extract the AST to OCaml values.

  The functions from this file are intended to be called from OCaml.
  See Clang_parser.mli
*/


/* Clang includes */
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/Version.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/RecordLayout.h"
#include "clang/AST/RawCommentList.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/ParseAST.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"

#if CLANG_VERSION_MAJOR < 17
#include "llvm/Support/Host.h"
#else
#include "llvm/TargetParser/Host.h"
#endif

#if CLANG_VERSION_MAJOR >= 10
#include "clang/Basic/Builtins.h"
#endif

/* OCaml includes */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/gc.h>

#undef flush // to be able to use std::flush

/* Other includes */
#include <iostream>
#include <unordered_map>

using namespace clang;


/* whether to log extra info (on cout) when throwing an OCaml exception */
static const bool verbose_exn = true;

/* log OCaml allocation (for debugging only) */
static const bool verbose_alloc = false;

/* whether to log cache statistics */
static const bool cache_statistics = false;

/* log when emitting an unknown node */
static const bool log_unknown = true;

#ifndef CLANGRESOURCE
#error "CLANGRESOURCE must be defined, e.g., -DCLANGRESOURCE=/usr/lib/clang/5.0.0"
#endif

#if CLANG_VERSION_MAJOR >= 8
#define DEBUG_SOURCE_RANGE(node) " at " << node->getSourceRange().printToString(src)
#else
#define DEBUG_SOURCE_RANGE(node) ""
#endif

#if CLANG_VERSION_MAJOR >= 17
#define Optional std::optional
#endif

/* OCaml helpers */
/* ************* */


#define CAML_EXPORT CAMLprim extern "C"

/* Used when debugging to catch allocation errors early */
CAML_EXPORT value caml_gc_full_major(value);

/* add support for defining CAMLlocals inside blocks */
#define CAMLenterblock() CAMLparam0()
#define CAMLexitblock()  CAMLdrop


/* dumps a OCaml block, for debugging */
static void dump_block(int ind, int depth, std::set<value>& found, value v) {
  if (Is_long(v)) {
    std::cout << "[long:" << Long_val(v) << "]" << std::flush;
  }
  else {
    std::cout << "[block@" << std::hex << v << std::dec << ":"  << std::flush;
    std::cout << int(Tag_val(v)) << "{" <<  Wosize_val(v) << "}" << std::flush;
    if (Tag_val(v) < No_scan_tag) {
      for (size_t i = 0; i < Wosize_val(v); i++) {
        value f = Field(v,i);
        if (Is_long(f)) {
          std::cout << "[long:" << Long_val(f) << "]" << std::flush;
        }
        else if (depth != 0 && !found.count(f)) {
          std::cout << std::endl;
          for (int i=0; i<ind; i++) std::cout << "| ";
          std::cout << "|-";

          dump_block(ind+1, depth > 0 ? depth-1 : depth, found, f);
        }
        else {
          std::cout << "@" << std::hex << f << std::dec << ";" << std::flush;
        }
      }
      std::cout << std::endl;
      for (int i=0; i<ind; i++) std::cout << "| ";
      std::cout << "]" << std::flush;
    }
  }
}

CAML_EXPORT value mlclang_dump_block(value recursive, value v) {
  CAMLparam2(recursive, v);
  std::set<value> found;
  dump_block(0, Bool_val(recursive) ? 100 : 0, found, v);
  std::cout << std::endl;
  CAMLreturn(Val_unit);
}


/* Cache */
/* ***** */

/* void* / unsigned -> value cache

   We aggressively cache every Clang object that is translated into OCaml to:
   - keep sharing of the AST at the OCaml level
   - handle cyclic structures

   Note: as of now, a cache is local to a translation unit.
 */
class Cache {

private:

  std::string name;

  std::unordered_map<uintptr_t,size_t> map;
  /* underlying map. from key to index in mlvalues */

  value mlvalues;
  /* all OCaml values are store into a single contiguous OCaml block,
     the GC can easily traverse the block and update values
  */

  size_t nb;
  /* allocated elements in mlvalues */

  size_t nb_queries, nb_hit;
  /* number of queries and acche hit, for statistic */

public:

  Cache(std::string name) : name(name), nb(0), nb_queries(0), nb_hit(0) {
    mlvalues = caml_alloc_tuple(100);
    caml_register_generational_global_root(&mlvalues);
  }

  ~Cache() {
    caml_remove_generational_global_root(&mlvalues);
    if (cache_statistics) dump_statistics();
  }

  void dump_statistics() {
    if (nb_queries) {
      std::cout << "cache '" << name << "', " <<  nb << " allocated, ";
      std::cout << nb_queries << " queries, " << nb_hit << " hit (";
      std::cout << (nb_hit*100/nb_queries) << "%)" << std::endl;
    }
    else {
      std::cout << "cache '" << name << "' not used" << std::endl;
    }
  }

  CAMLprim bool contains(uintptr_t key) {
    bool hit = map.count(key) > 0;
    if (cache_statistics) {
      nb_queries++;
      if (hit) nb_hit++;
    }
    return hit;
  }

  CAMLprim value get(uintptr_t key) {
    return Field(mlvalues, map[key]);
  }

  CAMLprim void store(uintptr_t key, value v) {
    CAMLparam1(v);
    CAMLlocal1(tmp);
    if (contains(key)) {
      Store_field(mlvalues, map[key], v);
    }
    else {
      map[key] = nb;
      if (nb >= Wosize_val(mlvalues)) {
        tmp = caml_alloc_tuple(2 * Wosize_val(mlvalues));
        for (size_t i = 0; i < Wosize_val(mlvalues); i++) {
          Store_field(tmp, i, Field(mlvalues, i));
        }
        caml_modify_generational_global_root(&mlvalues, tmp);
      }
      Store_field(mlvalues, nb, v);
      nb++;
    }
    CAMLreturn0;
  }

  CAMLprim void uncache(uintptr_t key) {
    Store_field(mlvalues, map[key], Val_unit);
    map.erase(key);
  }

  CAMLprim  bool contains(const void * key) {
    return contains(reinterpret_cast<uintptr_t>(key));
  }

  CAMLprim value get(const void * key) {
    return get(reinterpret_cast<uintptr_t>(key));
  }

  CAMLprim void store(const void * key, value v) {
    store(reinterpret_cast<uintptr_t>(key), v);
  }

  CAMLprim void uncache(const void * key) {
    uncache(reinterpret_cast<uintptr_t>(key));
  }

  CAMLprim value get_values() {
    CAMLparam0();
    CAMLlocal3(head, tmp, val);
    head = Val_unit;
    for (size_t i = 0; i < nb; i++) {
      val = Field(mlvalues, i);
      if (val != Val_unit) {
        tmp = caml_alloc_tuple(2);
        Store_field(tmp, 0, val);
        Store_field(tmp, 1, head);
        head = tmp;
      }
    }
    CAMLreturn (head);
  }
};



/* UTILITIES */
/*************/

/*
  NOTE ON MACROS:
  the preprocessor only balances ( ) in macro arguments, not { }
  => beware of commas in macros arguments
  MACRO( { int x,y; } ) will not work
  you have to do
  MACRO( { int x; int y; } )

  (also applies to CAMLlocalX for X > 1, that uses a comma and should not be used)

 */


#define check_null(ptr, fn)                                             \
  if (!ptr) { std::cerr << "mlClangAST: null pointer for " #ptr " in " fn << std::endl; abort(); }


/* execute BODY to get RES and cache,
   unless already associated with KEY in CACHE
*/
#define CACHED(CACHE, RES, KEY, BODY) {                                 \
    if (CACHE.contains(KEY)) {                                          \
      RES = CACHE.get(KEY);                                             \
    }                                                                   \
    else {                                                              \
      BODY;                                                             \
      CACHE.store(KEY, RES);                                            \
      if (verbose_alloc) std::cout << "  " << std::hex << RES << std::dec << " in cache " #CACHE << std::endl; \
    }                                                                   \
  }


/* allocates an OCaml block of size NBML with tag MLTAG, fills it
   with BODY, stores it into RES

   the block is associated with NODE in the cache but, if a block is
   already associated with NODE, it is used instead of allocating a new one

   NOTE: a block is put in the cache after it is allocated but _before_ it
   is filled with BODY; this allows cyclic data-structures to be handled
   correctly
*/
#define WITH_CACHE(CACHE, RES, NODE, NBML, MLTAG, BODY) {              \
    if (CACHE.contains(NODE))                                  \
      RES = CACHE.get(NODE);                                            \
    else {                                                              \
      RES = caml_alloc(NBML, MLTAG);                                    \
      if (verbose_alloc) std::cout << "  " << std::hex << RES << std::dec << " in cache " #CACHE << " tuple " << NBML << ", " << MLTAG << std::endl; \
      CACHE.store(NODE, RES);                                           \
      BODY;                                                             \
    }                                                                   \
  }

/* WITH_CACHE version with 0 tag (tuple, array, record) */
#define WITH_CACHE_TUPLE(CACHE, RES, NODE, NBML, BODY) \
  WITH_CACHE(CACHE, RES, NODE, NBML, 0, BODY)


/* creates a block with given tag and size, but only if it matches the expected type */
#define GENERATE_NODE(TYPE, RES, NODE, NBML, BODY)                      \
  if (RES == Val_int(-1) && isa<TYPE>(NODE)) {                          \
    const TYPE* x = cast<TYPE>(NODE);                                   \
    (void)x;                                                            \
    RES = caml_alloc(NBML, MLTAG_##TYPE);                               \
    if (verbose_alloc) std::cout << "  " << std::hex << RES << std::dec << " node " #TYPE << " tuple " << NBML << std::endl; \
    BODY;                                                               \
  }


/* cached version of GENERATE_NODE */
#define GENERATE_NODE_CACHED(CACHE, TYPE, RES, NODE, NBML, BODY)        \
  if (RES == Val_int(-1) && isa<TYPE>(NODE)) {                          \
    if (CACHE.contains(NODE)) {                                         \
        RES = CACHE.get(NODE);                                          \
      }                                                                 \
      else {                                                            \
        const TYPE* x = cast<TYPE>(NODE);                               \
        (void)x;                                                        \
        RES = caml_alloc(NBML, MLTAG_##TYPE);                           \
        if (verbose_alloc) std::cout << "  " << std::hex << RES << std::dec << " in cache " #CACHE " node " #TYPE << " tuple " << NBML << std::endl; \
        CACHE.store(NODE, RES);                                         \
        BODY;                                                           \
      }                                                                 \
  }


/* as GENERATE_NODE, but returns a variant of given tag with a single
   argument containing a struct with NBML elements
 */
#define GENERATE_NODE_INDIRECT(TYPE, RES, NODE, NBML, BODY)             \
  if (RES == Val_int(-1) && isa<TYPE>(NODE)) {                          \
    CAMLenterblock();                                                   \
    CAMLlocal1(tmp_);                                                   \
    const TYPE* x = cast<TYPE>(NODE);                                   \
    (void)x;                                                            \
    RES = caml_alloc_tuple(NBML);                                       \
    if (verbose_alloc) std::cout << "  " << std::hex << RES << std::dec << " node " #TYPE << " tuple " << NBML << std::endl; \
    BODY;                                                               \
    tmp_ = RES;                                                         \
    RES = caml_alloc(1, MLTAG_##TYPE);                                  \
    Store_field(RES, 0, tmp_);                                          \
    CAMLexitblock();                                                    \
  }


/* version of GENERATE_NODE for constant variants */
#define GENERATE_NODE_CONSTANT(TYPE, RES, NODE)                         \
  if (RES == Val_int(-1) && isa<TYPE>(NODE)) {                          \
    RES = Val_int(MLTAG_##TYPE);                                        \
  }



/* generic macro to generate list into RES from iterator ITERATOR executing CHILD */
#define GENERATE_LIST(RES, ITERATOR, CHILD)  {                          \
    CAMLenterblock();                                                   \
    value head_ = Val_unit;                                             \
    value last_ = Val_unit;                                             \
    value next_ = Val_unit;                                             \
    CAMLxparam3(head_, last_, next_);                                   \
    head_ = last_ = caml_alloc_tuple(2);                                \
    Store_field(last_, 1, Val_false);                                   \
    for (auto child : ITERATOR) {                                       \
      next_ = caml_alloc_tuple(2);                                      \
      Store_field(next_, 0, CHILD);                                     \
      Store_field(next_, 1, Val_false);                                 \
      Store_field(last_, 1, next_);                                     \
      last_ = next_;                                                    \
    }                                                                   \
    RES = Field(head_, 1);                                              \
    CAMLexitblock();                                                    \
  }

#define Store_field_list(RES, POS, ITERATOR, CHILD)  {          \
    CAMLenterblock();                                           \
    CAMLlocal1(tmp_);                                           \
    GENERATE_LIST(tmp_, ITERATOR, CHILD);                       \
    Store_field(RES, POS, tmp_);                                \
    CAMLexitblock();                                            \
  }

/* generic macro to generate arrays into RES, calling CHILD from i=0 to NB-1 */
#define GENERATE_ARRAY(RES, NB, CHILD) {             \
    int nb = NB;                                     \
    RES = (nb > 0) ? caml_alloc_tuple(nb) : Atom(0); \
    for (int i=0; i<nb; i++) {                       \
      Store_field(RES, i, CHILD);                    \
    }                                                \
  }

#define Store_field_array(RES, POS, NB , CHILD)  {               \
    CAMLenterblock();                                            \
    CAMLlocal1(tmp_);                                            \
    GENERATE_ARRAY(tmp_, NB, CHILD);                             \
    Store_field(RES, POS, tmp_);                                 \
    CAMLexitblock();                                             \
  }

#define Store_uid(RES, POS) \
  Store_field(RES, POS, Val_int(uid++))

/* generic macro to generate an option type: Some SOME if TEST is true, None otherwise */
#define GENERATE_OPTION(RES, TEST, SOME) {                 \
    if (TEST) {                                            \
      RES = caml_alloc_tuple(1);                                \
      Store_field(RES, 0, SOME);                                \
    }                                                           \
    else RES = Val_false;                                      \
  }

#define Store_field_option(RES, POS, TEST, SOME) {            \
    CAMLenterblock();                                            \
    CAMLlocal1(tmp_);                                            \
    GENERATE_OPTION(tmp_, TEST, SOME);                           \
    Store_field(RES, POS, tmp_);                                 \
    CAMLexitblock();                                             \
  }


/* helper for switch translating C++ enum to OCaml constant sum type */
#define GENERATE_CASE(RES, CASE)                \
  case CASE: RES = MLTAG_##CASE; break

#define GENERATE_CASE_PREFIX(RES, CPREFIX, MLPREFIX, CASE)      \
  case CPREFIX CASE: RES = MLTAG_##MLPREFIX##CASE; break

#define GENERATE_CASE_PREFIX_ALT(RES, CPREFIX, MLPREFIX, CCASE, MLCASE)        \
  case CPREFIX CCASE: RES = MLTAG_##MLPREFIX##MLCASE; break

#define GENERATE_CASE_PREFIX_REV(RES, CPREFIX, MLPREFIX, CASE)          \
  case Val_int(MLTAG_##MLPREFIX##CASE): RES = CPREFIX CASE; break




/* Locations */
/*********** */


/* The AST has a large number of location information.
   We try to cache them aggressively.
 */

class MLLocationTranslator {

 private:
  SourceManager& src;
  const LangOptions &opts;
  Cache cacheLoc;
  Cache cacheFile;
  value invalid_file;

public:
  CAMLprim value TranslateSourceLocation(SourceLocation a, int offset = 0);
  CAMLprim value TranslateSourceRange(SourceRange a);

  MLLocationTranslator(SourceManager& src, const LangOptions &opts)
    : src(src), opts(opts), cacheLoc("location"), cacheFile("filename") {
    invalid_file = caml_copy_string("<invalid>");
    caml_register_global_root(&invalid_file);
  }

  ~MLLocationTranslator() {
    caml_remove_global_root(&invalid_file);
  }

  CAMLprim value getFiles() { return cacheFile.get_values(); }

};

CAMLprim value MLLocationTranslator::TranslateSourceLocation(SourceLocation a, int offset) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);
  unsigned raw = a.getRawEncoding();
  PresumedLoc loc;  
  CACHED(cacheLoc, ret, raw, {
      loc = src.getPresumedLoc(a);
      ret = caml_alloc_tuple(3);

      if (loc.isValid()) {
        const char* filename = loc.getFilename();
        if (cacheFile.contains(filename)) {
          tmp = cacheFile.get(filename);
        }
        else {
          tmp = caml_copy_string(filename);
          cacheFile.store(filename, tmp);
        }
        // Clang counts lines & columns starting from 1
        // we count lines from 1 but columns from 0
        Store_field(ret, 0, Val_int(loc.getLine()));
        Store_field(ret, 1, Val_int(loc.getColumn()-1+offset));
        Store_field(ret, 2, tmp);
      }
      else {
        Store_field(ret, 0, Val_int(-1));
        Store_field(ret, 1, Val_int(-1));
        Store_field(ret, 2, invalid_file);
      }

    });

  CAMLreturn(ret);
}


/* SourceRange -> range */
CAMLprim value MLLocationTranslator::TranslateSourceRange(SourceRange a) {
  CAMLparam0();
  CAMLlocal1(ret);
  ret = caml_alloc_tuple(2);
  // from the begining of the first token...
  Store_field(ret, 0, TranslateSourceLocation(a.getBegin()));
  // ...to the last character of the last token (if possible)
  SourceLocation end(clang::Lexer::getLocForEndOfToken(a.getEnd(),0,src,opts));
  if (src.getPresumedLoc(end).isValid()) {
    Store_field(ret, 1, TranslateSourceLocation(end));
  }
  else {
    Store_field(ret, 1, TranslateSourceLocation(a.getEnd(), 1));
  }
  CAMLreturn(ret);
}




/* Comments */
/********** */


class MLCommentTranslator {

 private:
  SourceManager& src;
  MLLocationTranslator& loc;

public:
  CAMLprim value TranslateCommentKind(RawComment::CommentKind c);
  CAMLprim value TranslateRawComment(const RawComment *x);
  CAMLprim value TranslateRawCommentOpt(const RawComment *x);
  CAMLprim value getRawCommentList(ASTContext& Context);

  MLCommentTranslator(SourceManager& src, MLLocationTranslator& loc)
    : src(src), loc(loc)
  {}
};


/* comment_kind */
enum {
  MLTAG_RCK_Invalid,
  MLTAG_RCK_OrdinaryBCPL,
  MLTAG_RCK_OrdinaryC,
  MLTAG_RCK_BCPLSlash,
  MLTAG_RCK_BCPLExcl,
  MLTAG_RCK_JavaDoc,
  MLTAG_RCK_Qt,
  MLTAG_RCK_Merged,
};

/* CommentKind -> comment_kind */
CAMLprim value MLCommentTranslator::TranslateCommentKind(RawComment::CommentKind c) {
  int i = 0;
  switch (c) {
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_Invalid);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_OrdinaryBCPL);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_OrdinaryC);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_BCPLSlash);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_BCPLExcl);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_JavaDoc);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_Qt);
    GENERATE_CASE_PREFIX(i, RawComment::,, RCK_Merged);
  default:
    if (verbose_exn) std::cout << "unknown comment kind " << c << std::endl;
    caml_failwith("mlClangAST: unknown comment kind");
  }
  return Val_int(i);
}

/* RawComment -> comment */
CAMLprim value MLCommentTranslator::TranslateRawComment(const RawComment *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "RawComment");
  ret = caml_alloc_tuple(3);
  Store_field(ret, 0, caml_copy_string(x->getRawText(src).str().c_str()));
  Store_field(ret, 1, TranslateCommentKind(x->getKind()));
  Store_field(ret, 2, loc.TranslateSourceRange(x->getSourceRange()));

  CAMLreturn(ret);
}

/* RawComment (possibly NULL) -> comment list (0- or 1-length) */
CAMLprim value MLCommentTranslator::TranslateRawCommentOpt(const RawComment *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = Val_emptylist;
  if (x) {
    ret = caml_alloc_tuple(2);
    Store_field(ret, 0, TranslateRawComment(x));
    Store_field(ret, 1, Val_emptylist);
  }
  
  CAMLreturn(ret);
}

CAMLprim value MLCommentTranslator::getRawCommentList(ASTContext& Context) {
  CAMLparam0();
  CAMLlocal1(ret);
#if CLANG_VERSION_MAJOR < 10
  GENERATE_LIST(ret, Context.getRawCommentList().getComments(), TranslateRawComment(child));
#else
  std::vector<RawComment*> c;
  FileID id = Context.getSourceManager().getMainFileID();
  auto coms = Context.Comments.getCommentsInFile(id);
  if (coms != nullptr) {
    for (auto cc : *coms) {
      c.push_back(cc.second);
    }
  }
  GENERATE_LIST(ret, c, TranslateRawComment(child));
#endif
  CAMLreturn(ret);
}



/* AST classes */
/************* */


/*
  We use the RecursiveASTVisitor API to traverse the AST and build OCaml
  values on the fly.
 */


class MLTreeBuilderVisitor
  : public RecursiveASTVisitor<MLTreeBuilderVisitor> {

private:
  SourceManager& src;
  ASTContext *Context;
  MLLocationTranslator& loc;
  MLCommentTranslator& com;
  Cache cacheType;
  Cache cacheTypeQual;
  Cache cacheDecl;
  Cache cacheStmt;
  Cache cacheExpr;
  Cache cacheMisc;
  Cache cacheMisc2;
  Cache cacheMisc3;
  int uid;

public:
  CAMLprim value TranslateAPInt(const llvm::APInt & i);
  CAMLprim value TranslateAPSInt(const llvm::APSInt & i);
  CAMLprim value TranslateAPFloat(const llvm::APFloat & f);
  CAMLprim value TranslateEnumConstantDecl(const EnumConstantDecl* x);
  CAMLprim value TranslateEnumDecl(const EnumDecl * x);
  CAMLprim value TranslateRecordDecl(const RecordDecl * x);
  CAMLprim value TranslateFieldDecl(const FieldDecl * x);
  CAMLprim value TranslateArrayType(const ArrayType * x);
  CAMLprim value TranslateBuiltinType(const BuiltinType * node);
  CAMLprim value TranslateFunctionProtoType(const FunctionProtoType * x);
  CAMLprim value TranslateFunctionNoProtoType(const FunctionNoProtoType * x);
  CAMLprim value TranslateType(const Type * x);
  CAMLprim value TranslateTypeOption(const Type * x);
  CAMLprim value TranslateQualType(QualType x);
  CAMLprim value TranslateQualTypeOption(QualType x);
  CAMLprim value TranslateTypedefNameDecl(const TypedefNameDecl * x);
  CAMLprim value TranslateStorageClass(const StorageClass c);
  CAMLprim value TranslateStorageDuration(const StorageDuration c);
  CAMLprim value TranslateAccessSpecifier(AccessSpecifier c);
  CAMLprim value TranslateVarDecl(const VarDecl *node);
  CAMLprim value TranslateParmVarDecl(const ParmVarDecl *node);
  CAMLprim value TranslateFunctionDecl(const FunctionDecl *x);
  CAMLprim value TranslateBlockDecl(const BlockDecl *x);
  CAMLprim value TranslateCapture(const BlockDecl::Capture *x);
  CAMLprim value TranslateDecl(const Decl *node);
  CAMLprim value TranslateDeclContextChild(const DeclContext * ctx);
  CAMLprim value TranslateExpr(const Expr * e);
  CAMLprim value TranslateTypeTrait(TypeTrait trait, const Expr * node);
  CAMLprim value TranslateCXXConstructExpr(const CXXConstructExpr * node);
  CAMLprim value TranslateConstantIntegerExpr(const Expr * node);
  CAMLprim value TranslateOpaqueValueExpr(const OpaqueValueExpr * node);
  CAMLprim value TranslateDesignator(const DesignatedInitExpr * x, const DesignatedInitExpr::Designator * d);
  CAMLprim value TranslateOffsetOfNode(const OffsetOfExpr * x, const OffsetOfNode & n);
  CAMLprim value TranslateStmt(const Stmt * node);
  CAMLprim value TranslateCXXCatchStmt(const CXXCatchStmt * x);
  CAMLprim value TranslateDeclarationName(const DeclarationName& x);
  CAMLprim value TranslateNestedNameSpecifierLoc(const NestedNameSpecifierLoc& x);
  CAMLprim value TranslateNestedNameSpecifier(const NestedNameSpecifier* x);
  CAMLprim value TranslateNestedNameSpecifierList(const NestedNameSpecifier* x);
  CAMLprim value TranslateTemplateArgumentLoc(const TemplateArgumentLoc& x);
  CAMLprim value TranslateTemplateArgument(const TemplateArgument& x);
  CAMLprim value TranslateUnaryOperatorKind(UnaryOperatorKind k, const Expr * node);
  CAMLprim value TranslateBinaryOperatorKind(BinaryOperatorKind k, const Expr * node);
  CAMLprim value TranslateCompoundAssignOperatorKind(BinaryOperatorKind k, const Expr * node);
  CAMLprim value TranslateOverloadedOperatorKind(OverloadedOperatorKind k, const Expr * node);
#if CLANG_VERSION_MAJOR < 18
  CAMLprim value TranslateConstructionKind(CXXConstructExpr::ConstructionKind k, const Expr * x);
#else
  CAMLprim value TranslateConstructionKind(CXXConstructionKind k, const Expr * x);
#endif
  CAMLprim value TranslateTemplateParameterList(const TemplateParameterList *x);
  CAMLprim value TranslateNamespaceDecl(const NamespaceDecl *x);
  CAMLprim value TranslateNamespaceAliasDecl(const NamespaceAliasDecl *x);
  CAMLprim value TranslateClassTemplateDecl(const ClassTemplateDecl *x);
  CAMLprim value TranslateFunctionTemplateDecl(const FunctionTemplateDecl *x);
  CAMLprim value TranslateVarTemplateDecl(const VarTemplateDecl *x);
  CAMLprim value TranslateClassTemplateSpecializationDecl(const ClassTemplateSpecializationDecl *x);
  CAMLprim value TranslateVarTemplateSpecializationDecl(const VarTemplateSpecializationDecl *x);
  CAMLprim value TranslateFunctionTemplateSpecializationDecl(const FunctionDecl* x);
  CAMLprim value TranslateUsingDecl(const UsingDecl *x);
#if CLANG_VERSION_MAJOR >= 13
  CAMLprim value TranslateBaseUsingDecl(const BaseUsingDecl *x);
#endif
  CAMLprim value TranslateUsingPackDecl(const UsingPackDecl *x);
  CAMLprim value TranslateUsingShadowDecl(const UsingShadowDecl *x);
  CAMLprim value TranslateIndirectFieldDecl(const IndirectFieldDecl *x);
  CAMLprim value TranslateUnresolvedUsingValueDecl(const UnresolvedUsingValueDecl *x);
  CAMLprim value TranslateUnresolvedUsingTypenameDecl(const UnresolvedUsingTypenameDecl *x);
  CAMLprim value TranslateNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl *x);
  CAMLprim value TranslateTemplateTemplateParmDecl(const TemplateTemplateParmDecl *x);
  CAMLprim value TranslateTemplateName(const TemplateName& x);
  CAMLprim value TranslateCXXBaseSpecifier(const CXXBaseSpecifier& x);
  CAMLprim value TranslateFriendDecl(const FriendDecl *x);
  CAMLprim value TranslateRefQualifierKind(RefQualifierKind x);
  CAMLprim value TranslateCXXMethodDecl(const CXXMethodDecl *x);
  CAMLprim value TranslateCXXConstructorDecl(const CXXConstructorDecl *x);
  CAMLprim value TranslateCXXCtorInitializer(const CXXCtorInitializer *x);
  CAMLprim value TranslateLambdaCapture(const LambdaCapture& x);
  CAMLprim value TranslateTemplateTypeParmType(const TemplateTypeParmType* x);
  CAMLprim value TranslateTemplateTypeParmDecl(const TemplateTypeParmDecl* x);
  CAMLprim value TranslateNamedDecl(const NamedDecl *x);

  explicit MLTreeBuilderVisitor(MLLocationTranslator& loc, ASTContext *Context, SourceManager& src, MLCommentTranslator& com) :
    src(src), Context(Context), loc(loc), com(com),
    cacheType("type"), cacheTypeQual("type_qual"), cacheDecl("decl"), cacheStmt("stmt"),
    cacheExpr("expr"), cacheMisc("misc"), cacheMisc2("misc2"), cacheMisc3("misc3"),
    uid(0)
  {}

  bool TraverseDecl(Decl *node);
  bool TraverseStmt(Stmt *node);
  bool TraverseType(QualType node);

};



/* LITERALS */
/************/

extern "C" CAMLprim value ml_z_of_substring_base(value b, value v, value offset, value length);


/* APSInt -> Z.t */
CAMLprim value MLTreeBuilderVisitor::TranslateAPSInt(const llvm::APSInt & i) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);
  // TODO: be more efficient!
  SmallString<64> s;
  i.toString(s,16);
  tmp = caml_copy_string(s.c_str());
  ret = ml_z_of_substring_base(Val_int(16), tmp, Val_false, Val_int(caml_string_length(tmp)));
  CAMLreturn(ret);
}

/* APInt -> Z.t */
CAMLprim value MLTreeBuilderVisitor::TranslateAPInt(const llvm::APInt & i) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);
  // TODO: be more efficient!
  // we assume APInt to be unsigned; otherwise, the APSInt class would have been used instead
  SmallString<64> s;
  i.toStringUnsigned(s,16);
  tmp = caml_copy_string(s.c_str());
  ret = ml_z_of_substring_base(Val_int(16), tmp, Val_false, Val_int(caml_string_length(tmp)));
  CAMLreturn(ret);
}

/* APFloat -> string (in C99 hexadecimal format) */
CAMLprim value MLTreeBuilderVisitor::TranslateAPFloat(const llvm::APFloat & f) {
  // TODO: better buffer and rounding handling
  // our goal is to return an EXACT representation
  // and let the client convert and round if needed
  CAMLparam0();
  char buf[1024];
  f.convertToHexString(buf, 16, false, llvm::APFloat::rmNearestTiesToEven);
  CAMLreturn(caml_copy_string(buf));
}



/* DECLARATIONS */
/****************/


/* These enums should respect the order of definition of the corresponding
   ML union type
*/

/*  decl_kind */
enum {
  /* C */
  MLTAG_TranslationUnitDecl,
  MLTAG_FieldDecl,
  MLTAG_EnumConstantDecl,
  MLTAG_FileScopeAsmDecl,
  MLTAG_LinkageSpecDecl,
  MLTAG_LabelDecl,
  MLTAG_EnumDecl,
  MLTAG_RecordDecl,
  MLTAG_TypedefNameDecl,
  MLTAG_FunctionDecl,
  MLTAG_VarDecl,

  /* C++ */
  MLTAG_BlockDecl,
  MLTAG_FriendDecl,
  MLTAG_StaticAssertDecl,
  MLTAG_NamespaceAliasDecl,
  MLTAG_NamespaceDecl,
  MLTAG_BuiltinTemplateDecl,
  MLTAG_ClassTemplateDecl,
  MLTAG_FunctionTemplateDecl,
  MLTAG_TypeAliasTemplateDecl,
  MLTAG_VarTemplateDecl,
  MLTAG_TemplateTemplateParmDecl,
  MLTAG_TemplateTypeParmDecl,
  MLTAG_TypeAliasDecl,
  MLTAG_UnresolvedUsingTypenameDecl,
  MLTAG_UsingDecl,
  MLTAG_UsingDirectiveDecl,
  MLTAG_UsingPackDecl,
  MLTAG_UsingShadowDecl,
  MLTAG_BindingDecl,
  MLTAG_IndirectFieldDecl,
  MLTAG_UnresolvedUsingValueDecl,
  MLTAG_NonTypeTemplateParmDecl,

  MLTAG_UnknownDecl,
};

enum {
  /* C */
  MLTAG_EmptyDecl,

  /* C++ */
  MLTAG_AccessSpecDecl,
};

/* lang */
enum {
  MLTAG_LANG_C,
  MLTAG_LANG_CXX,
};

/* access_specifier */
enum {
  MLTAG_AS_public,
  MLTAG_AS_protected,
  MLTAG_AS_private,
  MLTAG_AS_none,
};

/* AccessSpecifier -> access_specifier */
CAMLprim value MLTreeBuilderVisitor::TranslateAccessSpecifier(AccessSpecifier c) {
  int i = 0;
  switch (c) {
    GENERATE_CASE(i, AS_public);
    GENERATE_CASE(i, AS_protected);
    GENERATE_CASE(i, AS_private);
    GENERATE_CASE(i, AS_none);
  default:
    if (verbose_exn) std::cout << "unknown access specifier " << c << std::endl;
    caml_failwith("mlClangAST: unknown access specifier");
  }
  return Val_int(i);
}

/* DeclContext -> decl list */
CAMLprim value MLTreeBuilderVisitor::TranslateDeclContextChild(const DeclContext* ctx) {
  CAMLparam0();
  CAMLlocal1(res);
  check_null(ctx, "TranslateDeclContextChild");
  GENERATE_LIST(res, ctx->decls(), TranslateDecl(child));
  CAMLreturn(res);
}

bool MLTreeBuilderVisitor::TraverseDecl(Decl *node) {
  return false;
}

/* builtin_template_kind */
enum {
  MLTAG_BTK__make_integer_seq,
  MLTAG_BTK__type_pack_element,
};

/* Decl -> decl */
CAMLprim value MLTreeBuilderVisitor::TranslateDecl(const Decl *node) {
  CAMLparam0();
  CAMLlocal2(ret,ret2);

  check_null(node, "TranslateDecl");

  if (verbose_alloc)
    std::cout << "TranslateDecl " << std::hex << node << std::dec << " " << node->getDeclKindName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  WITH_CACHE_TUPLE(cacheDecl, ret2, node, 4, {

      ret = Val_int(-1);

      // C nodes

      GENERATE_NODE(TranslationUnitDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateDeclContextChild(dyn_cast<DeclContext>(x)));
        });

      GENERATE_NODE(FieldDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateFieldDecl(x));
        });

      GENERATE_NODE(EnumConstantDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateEnumConstantDecl(x));
        });

      GENERATE_NODE_CONSTANT(EmptyDecl, ret, node);

      GENERATE_NODE(FileScopeAsmDecl, ret, node, 1, {
          Store_field(ret, 0, caml_copy_string(x->getAsmString()->getString().str().c_str()));
        });

      GENERATE_NODE(LinkageSpecDecl, ret, node, 2, {
          int r;
          switch (x->getLanguage()) {
#if CLANG_VERSION_MAJOR < 18
          case LinkageSpecDecl::lang_c: r = MLTAG_LANG_C; break;
          case LinkageSpecDecl::lang_cxx: r = MLTAG_LANG_C; break;
#else
          case LinkageSpecLanguageIDs::C: r = MLTAG_LANG_C; break;
          case LinkageSpecLanguageIDs::CXX: r = MLTAG_LANG_C; break;
#endif
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown language: " << static_cast<int>(x->getLanguage()) << DEBUG_SOURCE_RANGE(node) << std::endl; }
            caml_failwith("mlClangAST: unknown language in linkage specifier");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field(ret, 1, TranslateDeclContextChild(dyn_cast<DeclContext>(x)));
        });

      GENERATE_NODE(LabelDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateNamedDecl(x));
        });

      GENERATE_NODE(EnumDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateEnumDecl(x));
        });

      GENERATE_NODE(RecordDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateRecordDecl(x));
        });

      GENERATE_NODE(TypedefNameDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateTypedefNameDecl(x));
        });

      GENERATE_NODE(FunctionDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateFunctionDecl(x));
        });

      GENERATE_NODE(VarDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateVarDecl(x));
        });


      /* C++ */

      GENERATE_NODE_CONSTANT(AccessSpecDecl, ret, node);

      GENERATE_NODE(BlockDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateBlockDecl(x));
        });

      GENERATE_NODE(FriendDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateFriendDecl(x));
        });

      GENERATE_NODE_INDIRECT(StaticAssertDecl, ret, node, 3, {
          Store_field(ret, 0, TranslateExpr(x->getAssertExpr()));
          Store_field_option(ret, 1, x->getMessage(), TranslateExpr(x->getMessage()));
          Store_field(ret, 2, Val_bool(x->isFailed()));
        });

      GENERATE_NODE(NamespaceAliasDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateNamespaceAliasDecl(x));
        });

      GENERATE_NODE(NamespaceDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateNamespaceDecl(x));
        });

      GENERATE_NODE_INDIRECT(BuiltinTemplateDecl, ret, node, 5, {
          Store_field(ret, 0, TranslateNamedDecl(x));
          Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
          Store_field_option(ret, 2, x->getTemplatedDecl(), TranslateDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
          Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
          int r = 0;
          switch (x->getBuiltinTemplateKind()) {
            GENERATE_CASE(r, BTK__make_integer_seq);
            GENERATE_CASE(r, BTK__type_pack_element);
          default:
            if (verbose_exn) std::cout << "unknown builtin template: " << x->getBuiltinTemplateKind() << DEBUG_SOURCE_RANGE(node) << std::endl;
            caml_failwith("mlClangAST: unknown builtin template");
          }
          Store_field(ret, 4, Val_int(r));
        });

      GENERATE_NODE(ClassTemplateDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateClassTemplateDecl(x));
        });

      GENERATE_NODE(FunctionTemplateDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateFunctionTemplateDecl(x));
        });

      GENERATE_NODE_INDIRECT(TypeAliasTemplateDecl, ret, node, 4, {
          //x = x->getCanonicalDecl();
          Store_field(ret, 0, TranslateNamedDecl(x));
          Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
          Store_field(ret, 2, TranslateTypedefNameDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
          Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
        });

      GENERATE_NODE(VarTemplateDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateVarTemplateDecl(x));
        });

      GENERATE_NODE(TemplateTemplateParmDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateTemplateTemplateParmDecl(x));
        });

      GENERATE_NODE(TemplateTypeParmDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateTemplateTypeParmDecl(x));
        });

      GENERATE_NODE(TypeAliasDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateTypedefNameDecl(x));
        });

      GENERATE_NODE(UnresolvedUsingTypenameDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateUnresolvedUsingTypenameDecl(x));
        });

      GENERATE_NODE(UsingDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateUsingDecl(x));
        });

      GENERATE_NODE_INDIRECT(UsingDirectiveDecl, ret, node, 3, {
          Store_field(ret, 0, TranslateNamedDecl(x));
          Store_field(ret, 1, TranslateNestedNameSpecifierList(x->getQualifier()));
          Store_field(ret, 2, TranslateNamespaceDecl(x->getNominatedNamespace()));
        });

      GENERATE_NODE(UsingPackDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateUsingPackDecl(x));
        });

      GENERATE_NODE(UsingShadowDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateUsingShadowDecl(x));
        });

      GENERATE_NODE_INDIRECT(BindingDecl, ret, node, 4, {
          Store_field(ret, 0, TranslateNamedDecl(x));
          Store_field(ret, 1, TranslateQualType(x->getType()));
          Store_field(ret, 2, TranslateExpr(x->getBinding()));
          Store_field_option(ret, 3, x->getHoldingVar(), TranslateVarDecl(x->getHoldingVar()));
        });

      GENERATE_NODE(IndirectFieldDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateIndirectFieldDecl(x));
        });

      GENERATE_NODE(UnresolvedUsingValueDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateUnresolvedUsingValueDecl(x));
        });

      GENERATE_NODE(NonTypeTemplateParmDecl, ret, node, 1, {
          Store_field(ret, 0, TranslateNonTypeTemplateParmDecl(x));
        });


      // Default
      if (ret == Val_int(-1)) {
        ret = caml_alloc(2, MLTAG_UnknownDecl);
        //std::string s;
        //llvm::raw_string_ostream r(s);
        //node->print(r);
        Store_field(ret, 0, Val_int(node->getKind()));
        Store_field(ret, 1, caml_copy_string(node->getDeclKindName()/*r.str().c_str()*/));
        if (log_unknown) { std::cerr << "mlClangAST: unhandled Decl node encountered: " << node->getDeclKindName() << std::endl; }
      }

      Store_field(ret2, 0, ret);
      Store_field(ret2, 1, TranslateAccessSpecifier(node->getAccess()));
      Store_field(ret2, 2, loc.TranslateSourceRange(node->getSourceRange()));
      Store_field(ret2, 3, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(node)));
    });

  if (verbose_alloc)
    std::cout << "end TranslateDecl " << std::hex << node << std::dec << " " << node->getDeclKindName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  CAMLreturn(ret2);
}

/* NamedDecl -> name */
CAMLprim value MLTreeBuilderVisitor::TranslateNamedDecl(const NamedDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "NamedDecl");
  WITH_CACHE_TUPLE(cacheMisc3, ret, x, 3, {
      Store_field(ret, 0, caml_copy_string(x->getNameAsString().c_str()));
      Store_field(ret, 1, caml_copy_string(x->getQualifiedNameAsString().c_str()));
      Store_field(ret, 2, TranslateDeclarationName(x->getDeclName()));
    });

  CAMLreturn(ret);
}

/* UnresolvedUsingTypenameDecl -> unresolved_using_typename_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateUnresolvedUsingTypenameDecl(const UnresolvedUsingTypenameDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "UnresolvedUsingTypenameDecl");
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 3, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
      Store_field(ret, 2, Val_bool(x->isPackExpansion()));
    });

  CAMLreturn(ret);
}

/* FriendDecl -> friend_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateFriendDecl(const FriendDecl *org) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(org, "FriendDecl");
  const FriendDecl *x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 6, {
      Store_field_option(ret, 0, x->getFriendType(), TranslateQualType(x->getFriendType()->getType()));
      Store_field_option(ret, 1, x->getFriendDecl(), TranslateDecl(x->getFriendDecl()));
      Store_field(ret, 2, Val_bool(x->isUnsupportedFriend()));
      Store_field_array(ret, 3, x->getFriendTypeNumTemplateParameterLists(), TranslateTemplateParameterList(x->getFriendTypeTemplateParameterList(i)));
      Store_field(ret, 4, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 5, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
    });

  CAMLreturn(ret);
}

/* TemplateTemplateParmDecl -> template_template_param_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateTemplateParmDecl(const TemplateTemplateParmDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TemplateTemplateParmDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 8, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
      Store_field_option(ret, 2, x->getTemplatedDecl(), TranslateDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
      Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
      Store_field(ret, 4, Val_bool(x->isParameterPack()));
      Store_field(ret, 5, Val_bool(x->isPackExpansion()));
      if (x->isExpandedParameterPack()) {
        Store_field_array(ret, 6, x->getNumExpansionTemplateParameters(),
                          TranslateTemplateParameterList(x->getExpansionTemplateParameters(i)));
      }
      else Store_field(ret, 6, Atom(0));
      Store_field_option(ret, 7, x->hasDefaultArgument(), TranslateTemplateArgumentLoc(x->getDefaultArgument()));
    });

  CAMLreturn(ret);
}

/* NonTypeTemplateParmDecl -> non_type_template_parm_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "NonTypeTemplateParmDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 6, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateQualType(x->getType()));
      Store_field_option(ret, 2, x->hasDefaultArgument(), TranslateExpr(x->getDefaultArgument()));
      Store_field(ret, 3, Val_bool(x->isParameterPack()));
      Store_field(ret, 4, Val_bool(x->isPackExpansion()));
      if (x->isExpandedParameterPack()) {
        Store_field_array(ret, 5, x->getNumExpansionTypes(), TranslateQualType(x->getExpansionType(i)));
      }
      else Store_field(ret, 5, Atom(0));
    });

  CAMLreturn(ret);
}

/* UnresolvedUsingValueDecl -> unresolved_using_value_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateUnresolvedUsingValueDecl(const UnresolvedUsingValueDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "UnresolvedUsingValueDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 5, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateQualType(x->getType()));
      Store_field(ret, 2, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
      Store_field(ret, 3, Val_bool(x->isAccessDeclaration()));
      Store_field(ret, 4, Val_bool(x->isPackExpansion()));
    });

  CAMLreturn(ret);
}

/* IndirectFieldDecl -> indirect_field_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateIndirectFieldDecl(const IndirectFieldDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "IndirectFieldDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 5, {
      ArrayRef<NamedDecl*> d = x->chain();
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateQualType(x->getType()));
      Store_field(ret, 2, TranslateFieldDecl(x->getAnonField()));
      Store_field_option(ret, 3, x->getVarDecl(), TranslateVarDecl(x->getVarDecl()));
      Store_field_array(ret, 4, d.size(), TranslateDecl(d[i]));
    });

  CAMLreturn(ret);
}

/* UsingDecl -> using_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateUsingDecl(const UsingDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "UsingDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 3, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
      Store_field(ret, 2, Val_bool(x->hasTypename()));
    });

  CAMLreturn(ret);
}

#if CLANG_VERSION_MAJOR >= 13
/* BaseUsingDecl -> using_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateBaseUsingDecl(const BaseUsingDecl *x) {
  // TODO: handle other cases
  if (!isa<UsingDecl>(x))
    caml_failwith("mlClangAST: unhandled BasUsingDecl");
  return TranslateUsingDecl(cast<UsingDecl>(x));
}
#endif

/* UsingPackDecl -> using_pack_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateUsingPackDecl(const UsingPackDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "UsingPackDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 3, {
      ArrayRef<NamedDecl*> r = x->expansions();
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateDecl(x->getInstantiatedFromUsingDecl()));
      Store_field_array(ret, 2, r.size(), TranslateDecl(r[i]));
    });

  CAMLreturn(ret);
}

/* UsingShadowDecl -> using_shadow_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateUsingShadowDecl(const UsingShadowDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "UsingShadowDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 3, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateDecl(x->getTargetDecl()));
#if CLANG_VERSION_MAJOR < 13
      Store_field(ret, 2, TranslateUsingDecl(x->getUsingDecl()));
#else
      Store_field(ret, 2, TranslateBaseUsingDecl(x->getIntroducer()));
#endif
    });

  CAMLreturn(ret);
}

/* ClassTemplateDecl -> class_template_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateClassTemplateDecl(const ClassTemplateDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "ClassTemplateDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 6, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
      Store_field(ret, 2, TranslateRecordDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
      Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
      Store_field(ret, 4, Val_unit); //Store_field_list(ret, 4, x->specializations(), TranslateRecordDecl(child));
      Store_field(ret, 5, TranslateQualType((const_cast<ClassTemplateDecl*>(x))->getInjectedClassNameSpecialization()));
    });

  CAMLreturn(ret);
}

/* FunctionTemplateDecl -> function_template_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateFunctionTemplateDecl(const FunctionTemplateDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "FunctionTemplateDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 5, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
      Store_field(ret, 2, TranslateFunctionDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
      Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
      Store_field(ret, 4, Val_unit); //Store_field_list(ret, 4, x->specializations(), TranslateFunctionDecl(child));
    });

  CAMLreturn(ret);
}

/* VarTemplateDecl -> var_template_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateVarTemplateDecl(const VarTemplateDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "VarTemplateDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 5, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateTemplateParameterList(x->getTemplateParameters()));
      Store_field(ret, 2, TranslateVarDecl(x->getTemplatedDecl()));
#if CLANG_VERSION_MAJOR < 10
      Store_field_option(ret, 3, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
#else
          Store_field(ret, 3, Val_false);
#endif
      Store_field(ret, 4, Val_unit); //Store_field_list(ret, 4, x->specializations(), TranslateVarDecl(child));
    });

  CAMLreturn(ret);
}


/* ClassTemplateSpecializationDecl -> class_template_specialization */
CAMLprim value MLTreeBuilderVisitor::TranslateClassTemplateSpecializationDecl(const ClassTemplateSpecializationDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "ClassTemplateSpecializationDecl");
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 2, {
      const TemplateArgumentList& l = x->getTemplateArgs();
      Store_field(ret, 0, TranslateClassTemplateDecl(x->getSpecializedTemplate()));
      Store_field_array(ret, 1, l.size(), TranslateTemplateArgument(l.get(i)));
    });

  CAMLreturn(ret);
}

/* FunctionDecl -> function_template_specialization */
CAMLprim value MLTreeBuilderVisitor::TranslateFunctionTemplateSpecializationDecl(const FunctionDecl* x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "FunctionDecl");
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 2, {
      const TemplateArgumentList* l = x->getTemplateSpecializationArgs();
      Store_field(ret, 0, TranslateFunctionTemplateDecl(x->getPrimaryTemplate()));
      Store_field_array(ret, 1, l->size(), TranslateTemplateArgument(l->get(i)));
    });

  CAMLreturn(ret);
}

/* VarTemplateSpecializationDecl -> var_template_specialization */
CAMLprim value MLTreeBuilderVisitor::TranslateVarTemplateSpecializationDecl(const VarTemplateSpecializationDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "VarTemplateSpecializationDecl");
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 2, {
      const TemplateArgumentList& l = x->getTemplateArgs();
      Store_field(ret, 0, TranslateVarTemplateDecl(x->getSpecializedTemplate()));
      Store_field_array(ret, 1, l.size(), TranslateTemplateArgument(l.get(i)));
    });

  CAMLreturn(ret);
}


/* NamespaceDecl -> namespace_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateNamespaceDecl(const NamespaceDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "NamespaceDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 3, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, Val_bool(x->isAnonymousNamespace()));
      Store_field(ret, 2, Val_bool(x->isInline()));
    });

  CAMLreturn(ret);
}

/* NamespaceAliasDecl -> namespace_alias_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateNamespaceAliasDecl(const NamespaceAliasDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "NamespaceAliasDecl");
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 4, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field(ret, 1, TranslateNamespaceDecl(x->getNamespace()));
      Store_field(ret, 2, TranslateDecl(x->getAliasedNamespace()));
      Store_field(ret, 3, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
    });

  CAMLreturn(ret);
}

/* TemplateParameterList -> template_parameter_list */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateParameterList(const TemplateParameterList *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TemplateParameterDecl");
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 2, {
      Store_field_array(ret, 0, x->size(), TranslateDecl(x->getParam(i)));
      Store_field_option(ret, 1, x->getRequiresClause(), TranslateExpr(x->getRequiresClause()));
    });

  CAMLreturn(ret);
}

/* BlockDecl -> block_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateBlockDecl(const BlockDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "BlockDecl");
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 7, {
      Store_field_option(ret, 0, x->getBody(), TranslateStmt(x->getBody()));
      Store_field_array(ret, 1, x->getNumParams(), TranslateParmVarDecl(x->getParamDecl(i)));
      ArrayRef<BlockDecl::Capture> c = x->captures();
      Store_field_array(ret, 2, x->getNumCaptures(), TranslateCapture(&c[i]));
      Store_field(ret, 3, Val_bool(x->capturesCXXThis()));
      Store_field(ret, 4, Val_bool(x->isVariadic()));
      Store_field(ret, 5, Val_bool(x->blockMissingReturnType()));
      Store_field(ret, 6, Val_bool(x->isConversionFromLambda()));
    });

  CAMLreturn(ret);
}

/* BlockDecl::Capture -> capture */
CAMLprim value MLTreeBuilderVisitor::TranslateCapture(const BlockDecl::Capture *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "BlockDecl::Capture");
  WITH_CACHE_TUPLE(cacheDecl, ret, x, 4, {
      Store_field(ret, 0, TranslateVarDecl(x->getVariable()));
      Store_field(ret, 1, Val_bool(x->isByRef()));
      Store_field(ret, 2, Val_bool(x->isNested()));
      Store_field_option(ret, 3, x->hasCopyExpr(), TranslateExpr(x->getCopyExpr()));
    });

  CAMLreturn(ret);
}


/* storage_class */
enum {
  MLTAG_SC_None,
  MLTAG_SC_Extern,
  MLTAG_SC_Static,
  MLTAG_SC_PrivateExtern,
  MLTAG_SC_Auto,
  MLTAG_SC_Register,
};

/* StorageClass -> storage_class */
CAMLprim value MLTreeBuilderVisitor::TranslateStorageClass(StorageClass c) {
  int i = 0;
  switch (c) {
  GENERATE_CASE(i, SC_None);
  GENERATE_CASE(i, SC_Extern);
  GENERATE_CASE(i, SC_Static);
  GENERATE_CASE(i, SC_PrivateExtern);
  GENERATE_CASE(i, SC_Auto);
  GENERATE_CASE(i, SC_Register);
  default:
    if (verbose_exn) std::cout << "unknown storage class: " << c << std::endl;
    caml_failwith("mlClangAST: unknown storage class");
  }
  return Val_int(i);
}


/* storage_duration */
enum {
  MLTAG_SD_FullExpression,
  MLTAG_SD_Automatic,
  MLTAG_SD_Thread,
  MLTAG_SD_Static,
  MLTAG_SD_Dynamic,
};

/* StorageClass -> storage_class */
CAMLprim value MLTreeBuilderVisitor::TranslateStorageDuration(StorageDuration c) {
  int i = 0;
  switch (c) {
  GENERATE_CASE(i, SD_FullExpression);
  GENERATE_CASE(i, SD_Automatic);
  GENERATE_CASE(i, SD_Thread);
  GENERATE_CASE(i, SD_Static);
  GENERATE_CASE(i, SD_Dynamic);
  default:
    if (verbose_exn) std::cout << "unknown storage duration: " << c << std::endl;
    caml_failwith("mlClangAST: unknown storage duration");
  }
  return Val_int(i);
}

/* VarDecl -> var_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateVarDecl(const VarDecl *org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "TranslateVarDecl");
  const VarDecl *x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 10, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field(ret, 2, TranslateQualType(x->getType()));
      Store_field(ret, 3, TranslateStorageClass(x->getStorageClass()));
      Store_field_option(ret, 4, x->hasInit(), TranslateExpr(x->getInit()));
      Store_field(ret, 5, Val_bool(x->isFileVarDecl()));
      Store_field(ret, 6, Val_bool(x->isLocalVarDeclOrParm()));
      Store_field(ret, 7, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 8, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
      // template information
      Store_field_option(ret, 9, isa<VarTemplateSpecializationDecl>(x),
                         TranslateVarTemplateSpecializationDecl(cast<VarTemplateSpecializationDecl>(x)));
   });
  CAMLreturn(ret);
}

/* ParmVarDecl -> param_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateParmVarDecl(const ParmVarDecl *x) {
  /* for now, handle ParmVarDecl as VarDecl */
  return TranslateVarDecl(x);
}

// cxx_method_kind
enum {
  MLTAG_Method_Regular,
  MLTAG_Method_Destructor,
};

enum {
  MLTAG_Method_Constructor,
  MLTAG_Method_Conversion,
};

/* FunctionDecl -> function_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateFunctionDecl(const FunctionDecl *org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "FunctionDecl");
  const FunctionDecl *x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 15, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field_option(ret, 2, x->hasBody() && x->doesThisDeclarationHaveABody(), TranslateStmt(x->getBody()));
      Store_field(ret, 3, Val_bool(x->isVariadic()));
      Store_field(ret, 4, Val_bool(x->isMain()));
      Store_field(ret, 5, Val_bool(x->isGlobal()));
      Store_field(ret, 6, TranslateStorageClass(x->getStorageClass()));
      Store_field(ret, 7, TranslateQualType(x->getReturnType()));
      Store_field_array(ret, 8, x->getNumParams(), TranslateParmVarDecl(x->getParamDecl(i)));
      Store_field(ret, 9, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 10, loc.TranslateSourceRange(org->getNameInfo().getSourceRange()));
      Store_field(ret, 11, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
      // C++
      Store_field_option(ret, 12, x->getPrimaryTemplate (), TranslateFunctionTemplateSpecializationDecl(x));
      Store_field_option(ret, 13, x->isOverloadedOperator(), TranslateOverloadedOperatorKind(x->getOverloadedOperator(), NULL));
      Store_field_option(ret, 14, isa<CXXMethodDecl>(x), TranslateCXXMethodDecl(cast<CXXMethodDecl>(x)));
    });
  CAMLreturn(ret);
}

/* CXXMethodDecl -> cxx_method_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateCXXMethodDecl(const CXXMethodDecl *x) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(x, "CXXMethodDecl");
  ret = caml_alloc_tuple(6);
  Store_field(ret, 0, TranslateRecordDecl(x->getParent()));
  Store_field(ret, 1, Val_bool(x->isInstance()));
  Store_field(ret, 2, Val_bool(x->isVirtual()));
  Store_field_list(ret, 3, x->overridden_methods(), TranslateDecl(child));
  Store_field(ret, 4, TranslateRefQualifierKind(x->getRefQualifier()));
  tmp = Val_int(MLTAG_Method_Regular);
  if (isa<CXXConstructorDecl>(x)) {
    tmp = caml_alloc(1, MLTAG_Method_Constructor);
    Store_field(tmp, 0, TranslateCXXConstructorDecl(cast<CXXConstructorDecl>(x)));
  }
  else if (isa<CXXDestructorDecl>(x)) {
    tmp = Val_int(MLTAG_Method_Destructor);
  }
  else if (isa<CXXConversionDecl>(x)) {
    const CXXConversionDecl* y = cast<CXXConversionDecl>(x);
    tmp = caml_alloc(1, MLTAG_Method_Conversion);
    Store_field(tmp, 0, Val_bool(y->isExplicit()));
  }
  Store_field(ret, 5, tmp);

  CAMLreturn(ret);
}


  /* CXXConstructorDecl -> cxx_constructor_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateCXXConstructorDecl(const CXXConstructorDecl *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "CXXConstructorDecl");
  ret = caml_alloc_tuple(6);
  Store_field_list(ret, 0, x->inits(), TranslateCXXCtorInitializer(child));
  Store_field(ret, 1, Val_bool(x->isExplicit()));
  Store_field(ret, 2, Val_bool(x->isDelegatingConstructor()));
  Store_field(ret, 3, Val_bool(x->isInheritingConstructor()));
  Store_field_option(ret, 4, x->isDelegatingConstructor() && x->getTargetConstructor(), TranslateFunctionDecl(x->getTargetConstructor()));
  Store_field_option(ret, 5, x->getInheritedConstructor().getConstructor(), TranslateFunctionDecl(x->getInheritedConstructor().getConstructor()));
  CAMLreturn(ret);
}

enum {
  MLTAG_Constructor_init_Base,
  MLTAG_Constructor_init_Field,
  MLTAG_Constructor_init_Indirect_field,
  MLTAG_Constructor_init_Delegating,
};

/* CXXCtorInitializer -> cxx_constructor_initializer */
CAMLprim value MLTreeBuilderVisitor::TranslateCXXCtorInitializer(const CXXCtorInitializer *x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "CXXCtorInitializer");
  if (x->isBaseInitializer()) {
    ret = caml_alloc(3, MLTAG_Constructor_init_Base);
    Store_field(ret, 0, TranslateQualType(x->getTypeSourceInfo()->getType()));
    Store_field(ret, 1, Val_bool(x->isBaseVirtual()));
    Store_field(ret, 2, TranslateExpr(x->getInit()));
  }
  else if (x->isMemberInitializer()) {
    ret = caml_alloc(2, MLTAG_Constructor_init_Field);
    Store_field(ret, 0, TranslateFieldDecl(x->getMember()));
    Store_field(ret, 1, TranslateExpr(x->getInit()));
  }
  else if (x->isIndirectMemberInitializer()) {
    ret = caml_alloc(2, MLTAG_Constructor_init_Indirect_field);
    Store_field(ret, 0, TranslateIndirectFieldDecl(x->getIndirectMember()));
    Store_field(ret, 1, TranslateExpr(x->getInit()));
  }
  else if (x->isDelegatingInitializer()) {
    ret = caml_alloc(2, MLTAG_Constructor_init_Delegating);
    Store_field(ret, 0, TranslateQualType(x->getTypeSourceInfo()->getType()));
    Store_field(ret, 1, TranslateExpr(x->getInit()));
  }
  else {
    if (verbose_exn) std::cout << "unknown constructor initializer" << DEBUG_SOURCE_RANGE(x) << std::endl;
   caml_failwith("mlClangAST: unknown constructor initializer");
  }

  CAMLreturn(ret);
}

/* declaration_name */
enum {
  MLTAG_Identifier,
  MLTAG_CXXConstructorName,
  MLTAG_CXXDestructorName,
  MLTAG_CXXConversionFunctionName,
  MLTAG_CXXDeductionGuideName,
  MLTAG_CXXOperatorName,
  MLTAG_CXXLiteralOperatorName,
  MLTAG_CXXUsingDirective
};

CAMLprim value MLTreeBuilderVisitor::TranslateDeclarationName(const DeclarationName& x) {
  CAMLparam0();
  CAMLlocal1(ret);

  switch (x.getNameKind()) {
  case DeclarationName::Identifier:
    ret = caml_alloc(1, MLTAG_Identifier);
    Store_field(ret, 0, caml_copy_string(""/*x.getAsIdentifierInfo()->getNameStart()*/));
    break;
  case DeclarationName::CXXConstructorName:
    ret = caml_alloc(1, MLTAG_CXXConstructorName);
    Store_field(ret, 0, TranslateQualType(x.getCXXNameType()));
    break;
  case DeclarationName::CXXDestructorName:
    ret = caml_alloc(1, MLTAG_CXXDestructorName);
    Store_field(ret, 0, TranslateQualType(x.getCXXNameType()));
    break;
  case DeclarationName::CXXConversionFunctionName:
    ret = caml_alloc(1, MLTAG_CXXConversionFunctionName);
    Store_field(ret, 0, TranslateQualType(x.getCXXNameType()));
    break;
#if CLANG_VERSION_MAJOR >= 5
  case DeclarationName::CXXDeductionGuideName:
    ret = caml_alloc(1, MLTAG_CXXDeductionGuideName);
    Store_field(ret, 0, TranslateDecl(x.getCXXDeductionGuideTemplate()));
    break;
#endif
  case DeclarationName::CXXOperatorName:
    ret = caml_alloc(1, MLTAG_CXXOperatorName);
    Store_field(ret, 0, Val_false/*TranslateOverloadedOperatorKind(x.getCXXOverloadedOperator(), NULL)*/);
    break;
  case DeclarationName::CXXLiteralOperatorName:
    ret = caml_alloc(1, MLTAG_CXXLiteralOperatorName);
    Store_field(ret, 0, caml_copy_string(""/*x.getCXXLiteralIdentifier()->getNameStart()*/));
    break;
  case DeclarationName::CXXUsingDirective:
    ret = caml_alloc(1, MLTAG_CXXUsingDirective);
    Store_field(ret, 0, caml_copy_string("using directive"));
    break;
  default:
    if (verbose_exn) std::cout << "unknown declarationname kind " << x.getNameKind() << " " << x.getAsString() << std::endl;
    caml_failwith("mlClangAST: unknown declaration name kind");
  }

  CAMLreturn(ret);
}

/* NestedNameSpecifierLoc -> name_specifier_loc list */
CAMLprim value MLTreeBuilderVisitor::TranslateNestedNameSpecifierLoc(const NestedNameSpecifierLoc& x) {
  CAMLparam0();
  CAMLlocal4(head,last,next,tmp);
  NestedNameSpecifierLoc cur = x;
  head = last = caml_alloc_tuple(2);
  Store_field(last, 1, Val_false);
  while (cur.hasQualifier()) {
    tmp = caml_alloc_tuple(2);
    Store_field(tmp, 0, TranslateNestedNameSpecifier(cur.getNestedNameSpecifier()));
    Store_field(tmp, 1, loc.TranslateSourceRange(cur.getLocalSourceRange()));
    next = caml_alloc_tuple(2);
    Store_field(next, 0, tmp);
    Store_field(next, 1, Val_false);
    Store_field(last, 1, next);
    last = next;
    cur = cur.getPrefix();
  }
  CAMLreturn(Field(head, 1));
}

/* NestedNameSpecifier -> name_specifier list */
CAMLprim value MLTreeBuilderVisitor::TranslateNestedNameSpecifierList(const NestedNameSpecifier* x) {
  CAMLparam0();
  CAMLlocal3(head,last,next);
  head = last = caml_alloc_tuple(2);
  Store_field(last, 1, Val_false);
  while (x) {
    next = caml_alloc_tuple(2);
    Store_field(next, 0, TranslateNestedNameSpecifier(x));
    Store_field(next, 1, Val_false);
    Store_field(last, 1, next);
    last = next;
    x = x->getPrefix();
  }
  CAMLreturn(Field(head, 1));
}

enum {
  MLTAG_Name_specifier_Identifier,
  MLTAG_Name_specifier_Namespace,
  MLTAG_Name_specifier_NamespaceAlias,
  MLTAG_Name_specifier_TypeSpec,
  MLTAG_Name_specifier_TypeSpecWithTemplate,
};

enum {
  MLTAG_Name_specifier_Global,
};

/* NestedNameSpecifier -> name_specifier */
CAMLprim value MLTreeBuilderVisitor::TranslateNestedNameSpecifier(const NestedNameSpecifier* x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "NestedNameSpecifier");
  switch (x->getKind()) {
  case NestedNameSpecifier::Identifier:
    ret = caml_alloc(1, MLTAG_Name_specifier_Identifier);
    Store_field(ret, 0, caml_copy_string(x->getAsIdentifier()->getNameStart()));
    break;
  case NestedNameSpecifier::Namespace:
    ret = caml_alloc(1, MLTAG_Name_specifier_Namespace);
    Store_field(ret, 0, TranslateNamespaceDecl(x->getAsNamespace()));
    break;
  case NestedNameSpecifier::NamespaceAlias:
    ret = caml_alloc(1, MLTAG_Name_specifier_NamespaceAlias);
    Store_field(ret, 0, TranslateNamespaceAliasDecl(x->getAsNamespaceAlias()));
    break;
  case NestedNameSpecifier::TypeSpec:
    ret = caml_alloc(1, MLTAG_Name_specifier_TypeSpec);
    Store_field(ret, 0, TranslateType(x->getAsType()));
    break;
  case NestedNameSpecifier::TypeSpecWithTemplate:
    ret = caml_alloc(1, MLTAG_Name_specifier_TypeSpecWithTemplate);
    Store_field(ret, 0, TranslateType(x->getAsType()));
    break;
  case NestedNameSpecifier::Global:
    ret = Val_int(MLTAG_Name_specifier_Global);
    break;
  default:
    if (verbose_exn) x->dump();
    caml_failwith("mlClangAST: unknown kind of nested name specifier");
  }

  CAMLreturn(ret);
}

/* TemplateArgumentLoc -> template_argument * loc */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateArgumentLoc(const TemplateArgumentLoc& x) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, TranslateTemplateArgument(x.getArgument()));
  Store_field(ret, 1, loc.TranslateSourceRange(x.getSourceRange()));

  CAMLreturn(ret);
}

enum {
  MLTAG_Template_argument_Null,
};

enum {
  MLTAG_Template_argument_Type,
  MLTAG_Template_argument_Declaration,
  MLTAG_Template_argument_NullPtr,
  MLTAG_Template_argument_Integral,
  MLTAG_Template_argument_Template,
  MLTAG_Template_argument_Expression,
  MLTAG_Template_argument_Pack,
};

/* TemplateArgument -> template_argument */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateArgument(const TemplateArgument& x) {
  CAMLparam0();
  CAMLlocal1(ret);

  switch (x.getKind()) {
  case TemplateArgument::Null:
    ret = Val_int(MLTAG_Template_argument_Null);
    break;
  case TemplateArgument::Type:
    ret = caml_alloc(1, MLTAG_Template_argument_Type);
    Store_field(ret, 0, TranslateQualType(x.getAsType()));
    break;
  case TemplateArgument::Declaration:
    ret = caml_alloc(1, MLTAG_Template_argument_Declaration);
    Store_field(ret, 0, TranslateDecl(x.getAsDecl()));
    break;
  case TemplateArgument::NullPtr:
    ret = caml_alloc(1, MLTAG_Template_argument_NullPtr);
    Store_field(ret, 0, TranslateQualType(x.getNullPtrType()));
    break;
  case TemplateArgument::Integral:
    ret = caml_alloc(2, MLTAG_Template_argument_Integral);
    Store_field(ret, 0, TranslateQualType(x.getIntegralType()));
    Store_field(ret, 1, TranslateAPSInt(x.getAsIntegral()));
    break;
  case TemplateArgument::Template:
    ret = caml_alloc(1, MLTAG_Template_argument_Template);
    Store_field(ret, 0, TranslateTemplateName(x.getAsTemplate()));
    break;
  case TemplateArgument::Expression:
    ret = caml_alloc(1, MLTAG_Template_argument_Expression);
    Store_field(ret, 0, TranslateExpr(x.getAsExpr()));
    break;
  case TemplateArgument::Pack:
    {
      ArrayRef< TemplateArgument> a = x.getPackAsArray();
      ret = caml_alloc(2, MLTAG_Template_argument_Pack);
      Store_field(ret, 0, TranslateTemplateArgument(x.getPackExpansionPattern()));
      Store_field_array(ret, 1, a.size(), TranslateTemplateArgument(a[i]));
    }
    break;
  default:
    if (verbose_exn) x.dump();
    caml_failwith("mlClangAST: unknown kind of template argument");
  }

  CAMLreturn(ret);
}

enum {
  MLTAG_Template_name_Template,
  MLTAG_Template_name_OverloadedTemplate,
  MLTAG_Template_name_QualifiedTemplate,
  MLTAG_Template_name_DependentTemplate,
  MLTAG_Template_name_SubstTemplateTemplateParm,
  MLTAG_Template_name_SubstTemplateTemplateParmPack,
};

   /* TemplateName -> template_name */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateName(const TemplateName& x) {
  CAMLparam0();
  CAMLlocal1(ret);

  switch (x.getKind()) {
  case TemplateName::Template:
    ret = caml_alloc(1, MLTAG_Template_name_Template);
    Store_field(ret, 0, TranslateDecl(x.getAsTemplateDecl()));
    break;
  case TemplateName::OverloadedTemplate:
    {
      OverloadedTemplateStorage *a = x.getAsOverloadedTemplate();
      ret = caml_alloc(1, MLTAG_Template_name_OverloadedTemplate);
      Store_field_list(ret, 0, *a, TranslateDecl(child));
    }
    break;
  case TemplateName::QualifiedTemplate:
    {
      QualifiedTemplateName *a = x.getAsQualifiedTemplateName();
      ret = caml_alloc(2, MLTAG_Template_name_QualifiedTemplate);
      Store_field(ret, 0, TranslateNestedNameSpecifierList(a->getQualifier()));
#if CLANG_VERSION_MAJOR < 15
      Store_field(ret, 1, TranslateTemplateName(x.getUnderlying()));
#else
      Store_field(ret, 1, TranslateTemplateName(a->getUnderlyingTemplate()));
#endif
    }
    break;
  case TemplateName::DependentTemplate:
    {
      DependentTemplateName *a = x.getAsDependentTemplateName();
      ret = caml_alloc(3, MLTAG_Template_name_DependentTemplate);
      Store_field(ret, 0, TranslateNestedNameSpecifierList(a->getQualifier()));
      Store_field_option(ret, 1, a->isIdentifier(), caml_copy_string(a->getIdentifier()->getNameStart()));
      Store_field_option(ret, 2, a->isOverloadedOperator(), TranslateOverloadedOperatorKind(a->getOperator(), NULL));
    }
    break;
  case TemplateName::SubstTemplateTemplateParm:
    {
      SubstTemplateTemplateParmStorage *a = x.getAsSubstTemplateTemplateParm();
      ret = caml_alloc(2, MLTAG_Template_name_SubstTemplateTemplateParm);
      Store_field(ret, 0, TranslateDecl(a->getParameter()));
      Store_field(ret, 1, TranslateTemplateName(a->getReplacement()));
    }
    break;
  case TemplateName::SubstTemplateTemplateParmPack:
    {
      SubstTemplateTemplateParmPackStorage *a = x.getAsSubstTemplateTemplateParmPack();
      ret = caml_alloc(2, MLTAG_Template_name_SubstTemplateTemplateParmPack);
      Store_field(ret, 0, TranslateDecl(a->getParameterPack()));
      Store_field(ret, 1, TranslateTemplateArgument(a->getArgumentPack()));
    }
    break;
  default:
    if (verbose_exn) x.dump();
    caml_failwith("mlClangAST: unknown kind of template name");
  }

  CAMLreturn(ret);
}



/* EXPRESSIONS */
/***************/


/* expr_kind with argument */
enum {
  MLTAG_ConditionalOperator,
  MLTAG_BinaryConditionalOperator,
  MLTAG_AddrLabelExpr,
  MLTAG_ArrayInitLoopExpr,
  MLTAG_ArraySubscriptExpr,
  MLTAG_AtomicExpr,
  MLTAG_CompoundAssignOperator,
  MLTAG_BinaryOperator,
  MLTAG_UnaryOperator,
  MLTAG_CallExpr,
  MLTAG_CastExpr,
  MLTAG_CharacterLiteral,
  MLTAG_ChooseExpr,
  MLTAG_CompoundLiteralExpr,
  MLTAG_DeclRefExpr,
  MLTAG_DesignatedInitExpr,
  MLTAG_FloatingLiteral,
  MLTAG_GenericSelectionExpr,
  MLTAG_ImaginaryLiteral,
  MLTAG_InitListExpr,
  MLTAG_IntegerLiteral,
  MLTAG_MemberExpr,
  MLTAG_OffsetOfExpr,
  MLTAG_OpaqueValueExpr,
  MLTAG_ParenExpr,
  MLTAG_ParenListExpr,
  MLTAG_PredefinedExpr,
  MLTAG_PseudoObjectExpr,
  MLTAG_StmtExpr,
  MLTAG_StringLiteral,
  MLTAG_UnaryExprOrTypeTraitExpr,
  MLTAG_VAArgExpr,
  MLTAG_FullExpr,
  MLTAG_ConstantExpr,

  /* C++ expressions */
  MLTAG_ArrayTypeTraitExpr,
  MLTAG_CXXBindTemporaryExpr,
  MLTAG_CXXBoolLiteralExpr,
  MLTAG_CXXConstructExpr,
  MLTAG_CXXDefaultArgExpr,
  MLTAG_CXXDefaultInitExpr,
  MLTAG_CXXDeleteExpr,
  MLTAG_CXXDependentScopeMemberExpr,
  MLTAG_CXXFoldExpr,
  MLTAG_CXXInheritedCtorInitExpr,
  MLTAG_CXXNewExpr,
  MLTAG_CXXNoexceptExpr,
  MLTAG_CXXPseudoDestructorExpr,
  MLTAG_CXXStdInitializerListExpr,
  MLTAG_CXXThisExpr,
  MLTAG_CXXThrowExpr,
  MLTAG_CXXTypeidExpr,
  MLTAG_CXXUnresolvedConstructExpr,
  MLTAG_DependentScopeDeclRefExpr,
  MLTAG_ExpressionTraitExpr,
  MLTAG_ExprWithCleanups,
  MLTAG_FunctionParmPackExpr,
  MLTAG_MaterializeTemporaryExpr,
  MLTAG_PackExpansionExpr,
  MLTAG_SizeOfPackExpr,
  MLTAG_SubstNonTypeTemplateParmExpr,
  MLTAG_SubstNonTypeTemplateParmPackExpr,
  MLTAG_TypeTraitExpr,
  MLTAG_UnresolvedLookupExpr,
  MLTAG_UnresolvedMemberExpr,
  MLTAG_LambdaExpr,

  /* Vectors */
  MLTAG_ConvertVectorExpr,
  MLTAG_ExtVectorElementExpr,
  MLTAG_ShuffleVectorExpr,

  /* Unknown */
  MLTAG_UnknownExpr,
};

/* expr_kind without argument */
enum {
  MLTAG_ArrayInitIndexExpr,
  MLTAG_GNUNullExpr,
  MLTAG_ImplicitValueInitExpr,
  MLTAG_NoInitExpr,
  MLTAG_CXXNullPtrLiteralExpr,
  MLTAG_CXXScalarValueInitExpr,
};

/* cast_kind */
enum {
  MLTAG_CStyleCast,
  MLTAG_CXXFunctionalCast,
  MLTAG_CXXConstCast,
  MLTAG_CXXDynamicCast,
  MLTAG_CXXReinterpretCast,
  MLTAG_CXXStaticCast,
  MLTAG_ImplicitCast,
};

/* character_kind */
enum {
  MLTAG_Char_Ascii,
  MLTAG_Char_Wide,
  MLTAG_Char_UTF8,
  MLTAG_Char_UTF16,
  MLTAG_Char_UTF32,
  MLTAG_Char_Unevaluated, // for strings
};

/* ident_type */
enum {
  MLTAG_Ident_Func,
  MLTAG_Ident_Function,
  MLTAG_Ident_LFunction,
  MLTAG_Ident_FuncDName,
  MLTAG_Ident_FuncSig,
  MLTAG_Ident_PrettyFunction,
  MLTAG_Ident_PrettyFunctionNoVirtual,
};

/* unary_expr_or_type */
enum {
  MLTAG_UETT_SizeOf,
  MLTAG_UETT_AlignOf,
  // Clang >= 8
  MLTAG_UETT_PreferredAlignOf,
};

/* array_type_trait */
enum {
  MLTAG_ATT_ArrayRank,
  MLTAG_ATT_ArrayExtent,
};

/* initialization_style */
enum {
  MLTAG_New_NoInit,
  MLTAG_New_CallInit,
  MLTAG_New_ListInit,
};

/* expression_trait */
enum {
  MLTAG_ET_IsLValueExpr,
  MLTAG_ET_IsRValueExpr,
};

/* lambda_capture_default */
enum {
  MLTAG_LCD_None,
  MLTAG_LCD_ByCopy,
  MLTAG_LCD_ByRef,
};

/* Expr -> expr */
CAMLprim value MLTreeBuilderVisitor::TranslateExpr(const Expr * node) {
  CAMLparam0();
  CAMLlocal3(ret,ret2,tmp);

  check_null(node, "TranslateExpr");
  if (verbose_alloc)
    std::cout << "TranslateExpr " << std::hex << node << std::dec << " " << node->getStmtClassName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  WITH_CACHE_TUPLE(cacheExpr, ret2, node, 3, {

      ret = Val_int(-1);

      GENERATE_NODE_INDIRECT(ConditionalOperator, ret, node, 3, {
          Store_field(ret, 0, TranslateExpr(x->getCond()));
          Store_field(ret, 1, TranslateExpr(x->getTrueExpr()));
          Store_field(ret, 2, TranslateExpr(x->getFalseExpr()));
        });

      GENERATE_NODE_INDIRECT(BinaryConditionalOperator, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getCond()));
          Store_field(ret, 1, TranslateExpr(x->getFalseExpr()));
        });

      GENERATE_NODE(AddrLabelExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateNamedDecl(x->getLabel()->getStmt()->getDecl()));
        });

      GENERATE_NODE_CONSTANT(ArrayInitIndexExpr, ret, node);

      GENERATE_NODE_INDIRECT(ArrayInitLoopExpr, ret, node, 3, {
          Store_field(ret, 0, TranslateOpaqueValueExpr(x->getCommonExpr()));
          Store_field(ret, 1, TranslateExpr(x->getSubExpr()));
          Store_field(ret, 2, TranslateAPInt(x->getArraySize()));
        });

      GENERATE_NODE_INDIRECT(ArraySubscriptExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getBase()));
          Store_field(ret, 1, TranslateExpr(x->getIdx()));
        });

      GENERATE_NODE_INDIRECT(AtomicExpr, ret, node, 3, {
          Store_field(ret, 0, Val_int(x->getOp()));
          Store_field(ret, 1, TranslateExpr(x->getPtr()));
          Store_field(ret, 2, TranslateExpr(x->getOrder()));
        });

      /* keep before BinaryOperator; otherwise it would be matched by BinaryOperator instead */
      GENERATE_NODE_INDIRECT(CompoundAssignOperator, ret, node, 5, {
          Store_field(ret, 0, TranslateExpr(x->getLHS()));
          Store_field(ret, 1, TranslateQualType(x->getComputationLHSType()));
          Store_field(ret, 2, TranslateCompoundAssignOperatorKind(x->getOpcode(), x));
          Store_field(ret, 3, TranslateExpr(x->getRHS()));
          Store_field(ret, 4, TranslateQualType(x->getComputationResultType()));
       });

      GENERATE_NODE(BinaryOperator, ret, node, 3, {
          Store_field(ret, 0, TranslateExpr(x->getLHS()));
          Store_field(ret, 1, TranslateBinaryOperatorKind(x->getOpcode(), x));
          Store_field(ret, 2, TranslateExpr(x->getRHS()));
        });

      GENERATE_NODE(UnaryOperator, ret, node, 2, {
          Store_field(ret, 0, TranslateUnaryOperatorKind(x->getOpcode(), x));
          Store_field(ret, 1, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE_INDIRECT(CallExpr, ret, node, 4, {
          Store_field(ret, 0, TranslateExpr(x->getCallee()));
          Store_field_option(ret, 1, x->getDirectCallee(), TranslateFunctionDecl(x->getDirectCallee()));
          Store_field_array(ret, 2, x->getNumArgs(), TranslateExpr(x->getArg(i)));
          Store_field_option(ret, 3, isa<CXXOperatorCallExpr>(x), TranslateOverloadedOperatorKind((cast<CXXOperatorCallExpr>(x))->getOperator(),x));
        });

      GENERATE_NODE(CastExpr, ret, node, 2, {
          int r = 0;
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));

          if (isa<CStyleCastExpr>(x))r = MLTAG_CStyleCast;
          else if (isa<CXXFunctionalCastExpr>(x))  r = MLTAG_CXXFunctionalCast;
          else if (isa<CXXConstCastExpr>(x))       r = MLTAG_CXXConstCast;
          else if (isa<CXXDynamicCastExpr>(x))     r = MLTAG_CXXDynamicCast;
          else if (isa<CXXReinterpretCastExpr>(x)) r = MLTAG_CXXReinterpretCast;
          else if (isa<CXXStaticCastExpr>(x))      r = MLTAG_CXXStaticCast;
          else if (isa<ImplicitCastExpr>(x))       r = MLTAG_ImplicitCast;
          else {
            if (verbose_exn) node->dump();
            caml_failwith("mlClangAST: unknown kind of cast");
          }
          Store_field(ret, 1, Val_int(r));
        });

      GENERATE_NODE(CharacterLiteral, ret, node, 2, {
          int r = 0;
          Store_field(ret, 0, caml_copy_int32(x->getValue()));
          switch (x->getKind()) {
#if CLANG_VERSION_MAJOR < 18
            GENERATE_CASE_PREFIX(r, CharacterLiteral::, Char_, Ascii);
            GENERATE_CASE_PREFIX(r, CharacterLiteral::, Char_, Wide);
            GENERATE_CASE_PREFIX(r, CharacterLiteral::, Char_, UTF8);
            GENERATE_CASE_PREFIX(r, CharacterLiteral::, Char_, UTF16);
            GENERATE_CASE_PREFIX(r, CharacterLiteral::, Char_, UTF32);
#else
            GENERATE_CASE_PREFIX(r, CharacterLiteralKind::, Char_, Ascii);
            GENERATE_CASE_PREFIX(r, CharacterLiteralKind::, Char_, Wide);
            GENERATE_CASE_PREFIX(r, CharacterLiteralKind::, Char_, UTF8);
            GENERATE_CASE_PREFIX(r, CharacterLiteralKind::, Char_, UTF16);
            GENERATE_CASE_PREFIX(r, CharacterLiteralKind::, Char_, UTF32);
#endif
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown kind of character literal: " << static_cast<int>(x->getKind()) << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown kind of character literal");
          }
          Store_field(ret, 1, Val_int(r));
        });

      GENERATE_NODE_INDIRECT(ChooseExpr, ret, node, 5, {
          Store_field(ret, 0, TranslateExpr(x->getCond()));
          Store_field(ret, 1, TranslateExpr(x->getLHS()));
          Store_field(ret, 2, TranslateExpr(x->getRHS()));
          Store_field(ret, 3, TranslateExpr(x->getLHS()));
          Store_field(ret, 4, Val_bool(x->isConditionTrue()));
        });

      GENERATE_NODE(CompoundLiteralExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getInitializer()));
          Store_field(ret, 1, Val_bool(x->isFileScope()));
        });

      GENERATE_NODE(DeclRefExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateDecl(x->getDecl()));
        });

      GENERATE_NODE(DesignatedInitExpr, ret, node, 2, {
          Store_field_array(ret, 0, x->size(), TranslateDesignator(x, ((const_cast<DesignatedInitExpr*>(x))->getDesignator(i))));
          Store_field(ret, 1, TranslateExpr(x->getInit()));
        });

      GENERATE_NODE(FloatingLiteral, ret, node, 1, {
          Store_field(ret, 0, TranslateAPFloat(x->getValue()));
        });

      GENERATE_NODE_INDIRECT(GenericSelectionExpr, ret, node, 3, {
          Store_field(ret, 0, TranslateExpr(x->getControllingExpr()));
#if CLANG_VERSION_MAJOR >= 9
          Store_field_array(ret, 1, x->getNumAssocs(), TranslateExpr(x->getAssocExprs()[i]));
#else
          Store_field_array(ret, 1, x->getNumAssocs(), TranslateExpr(x->getAssocExpr(i)));
#endif
          Store_field(ret, 2, Val_int(x->getResultIndex()));
        });

      GENERATE_NODE_CONSTANT(GNUNullExpr, ret, node);

      GENERATE_NODE(ImaginaryLiteral, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE_CONSTANT(ImplicitValueInitExpr, ret, node);

      GENERATE_NODE_INDIRECT(InitListExpr, ret, node, 3, {
          Store_field_array(ret, 0, x->getNumInits(), TranslateExpr(x->getInit(i)));
          Store_field_option(ret, 1, x->getInitializedFieldInUnion(), TranslateFieldDecl(x->getInitializedFieldInUnion()));
          Store_field_option(ret, 2, x->hasArrayFiller(), TranslateExpr(x->getArrayFiller()));
        });

      GENERATE_NODE(IntegerLiteral, ret, node, 1, {
          Store_field(ret, 0, TranslateAPInt(x->getValue()));
        });

      GENERATE_NODE_INDIRECT(MemberExpr, ret, node, 6, {
          Store_field(ret, 0, TranslateExpr(x->getBase()));
          Store_field(ret, 1, TranslateDecl(x->getMemberDecl()));
          Store_field(ret, 2, Val_bool(x->isArrow()));
          Store_field_option(ret, 3, x->hasQualifier(), TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
          const TemplateArgumentLoc* tmp = x->getTemplateArgs();
          Store_field_array(ret, 4, x->getNumTemplateArgs(), TranslateTemplateArgumentLoc(tmp[i]));
          Store_field(ret, 5, TranslateDeclarationName(x->getMemberNameInfo().getName()));
        });

      GENERATE_NODE_CONSTANT(NoInitExpr, ret, node);

      GENERATE_NODE(OffsetOfExpr, ret, node, 2, {
          Store_field_array(ret, 0, x->getNumComponents(), TranslateOffsetOfNode(x, x->getComponent(i)));
          Store_field(ret, 1, TranslateConstantIntegerExpr(node));
        });

      GENERATE_NODE(OpaqueValueExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateOpaqueValueExpr(x));
        });

      GENERATE_NODE(ParenExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE(ParenListExpr, ret, node, 1, {
          Store_field_array(ret, 0, x->getNumExprs(), TranslateExpr(x->getExpr(i)));
        });

      GENERATE_NODE(PredefinedExpr, ret, node, 2, {
          int r = 0;
#if CLANG_VERSION_MAJOR >= 8
          switch (x->getIdentKind()) {
#else
          switch (x->getIdentType()) {
#endif
#if CLANG_VERSION_MAJOR < 18
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, Func);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, Function);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, LFunction);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, FuncDName);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, FuncSig);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, PrettyFunction);
            GENERATE_CASE_PREFIX(r, PredefinedExpr::, Ident_, PrettyFunctionNoVirtual);
#else
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, Func);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, Function);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, LFunction);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, FuncDName);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, FuncSig);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, PrettyFunction);
            GENERATE_CASE_PREFIX(r, PredefinedIdentKind::, Ident_, PrettyFunctionNoVirtual);
#endif
          default:
#if CLANG_VERSION_MAJOR >= 8
            if (verbose_exn) { node->dump(); std::cout << "unknown ident type: " << static_cast<int>(x->getIdentKind()) << DEBUG_SOURCE_RANGE(node) << std::endl; }
#else            
            if (verbose_exn) { node->dump(); std::cout << "unknown ident type: " << x->getIdentType() << DEBUG_SOURCE_RANGE(node) << std::endl; }
#endif
            caml_failwith("mlClangAST: unknown ident type");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field(ret, 1, caml_copy_string(x->getFunctionName() ? x->getFunctionName()->getString().str().c_str() : ""));
        });

      GENERATE_NODE_INDIRECT(PseudoObjectExpr, ret, node, 3, {
          Store_field(ret, 0, TranslateExpr(x->getSyntacticForm()));
          Store_field_array(ret, 1, x->getNumSemanticExprs(), TranslateExpr(x->getSemanticExpr(i)));
          if (x->getResultExprIndex() == PseudoObjectExpr::NoResult) {
            tmp = Val_false;
          }
          else {
            tmp = caml_alloc_tuple(1);
            Store_field(tmp, 0, Val_int(x->getResultExprIndex()));
          }
          Store_field(ret, 2, tmp);
        });

      GENERATE_NODE(StmtExpr, ret, node, 1, {
          Store_field_list(ret, 0, x->getSubStmt()->body(), TranslateStmt(child));
        });

      GENERATE_NODE(StringLiteral, ret, node, 2, {
          int r = 0;
          // manual copy, as x->getBytes() is not necessarily 0-terminated
          tmp = caml_alloc_string(x->getByteLength());
          memcpy(const_cast<char*>String_val(tmp), x->getBytes().data(), x->getByteLength());
          Store_field(ret, 0, tmp);
          switch (x->getKind()) {
#if CLANG_VERSION_MAJOR < 18
#if CLANG_VERSION_MAJOR >= 15
            GENERATE_CASE_PREFIX_ALT(r, StringLiteral::, Char_, Ordinary, Ascii);
#else
            GENERATE_CASE_PREFIX(r, StringLiteral::, Char_, Ascii);
#endif
            GENERATE_CASE_PREFIX(r, StringLiteral::, Char_, Wide);
            GENERATE_CASE_PREFIX(r, StringLiteral::, Char_, UTF8);
            GENERATE_CASE_PREFIX(r, StringLiteral::, Char_, UTF16);
            GENERATE_CASE_PREFIX(r, StringLiteral::, Char_, UTF32);
#else
            GENERATE_CASE_PREFIX_ALT(r, StringLiteralKind::, Char_, Ordinary, Ascii);
            GENERATE_CASE_PREFIX(r, StringLiteralKind::, Char_, Wide);
            GENERATE_CASE_PREFIX(r, StringLiteralKind::, Char_, UTF8);
            GENERATE_CASE_PREFIX(r, StringLiteralKind::, Char_, UTF16);
            GENERATE_CASE_PREFIX(r, StringLiteralKind::, Char_, UTF32);
            GENERATE_CASE_PREFIX(r, StringLiteralKind::, Char_, Unevaluated);
#endif
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown kind of string literal: " << static_cast<int>(x->getKind()) << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown kind of string literal");
          }
          Store_field(ret, 1, Val_int(r));
        });

      GENERATE_NODE(UnaryExprOrTypeTraitExpr, ret, node, 2, {
          int r = 0;
          switch (x->getKind()) {
            GENERATE_CASE(r, UETT_SizeOf);
            GENERATE_CASE(r, UETT_AlignOf);
#if CLANG_VERSION_MAJOR >= 8
            GENERATE_CASE(r, UETT_PreferredAlignOf);
#endif
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown kind of unary expression or type trait operator: " << x->getKind() << DEBUG_SOURCE_RANGE(node) << std::endl; }
            caml_failwith("mlClangAST: unknown kind of unary expresion or type trait operator");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field(ret, 1, TranslateQualType(x->getTypeOfArgument()));
        });

      GENERATE_NODE(VAArgExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });


      /* C++ */

      GENERATE_NODE_INDIRECT(ArrayTypeTraitExpr, ret, node, 4, {
          int r = 0;
          switch (x->getTrait()) {
            GENERATE_CASE(r, ATT_ArrayRank);
            GENERATE_CASE(r, ATT_ArrayExtent);
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown array type trait: " << x->getTrait() << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown array type trait");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field(ret, 1, TranslateQualType(x->getQueriedType()));
          Store_field(ret, 2, caml_copy_int64(x->getValue()));
          Store_field(ret, 3, TranslateExpr(x->getDimensionExpression()));
        });

      GENERATE_NODE(CXXBindTemporaryExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE(CXXBoolLiteralExpr, ret, node, 1, {
          Store_field(ret, 0, Val_bool(x->getValue()));
        });

      GENERATE_NODE(CXXConstructExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateCXXConstructExpr(x));
        });

      GENERATE_NODE(CXXDefaultArgExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateParmVarDecl(x->getParam()));
          Store_field(ret, 1, TranslateExpr(x->getExpr()));
        });

      GENERATE_NODE(CXXDefaultInitExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateFieldDecl(x->getField()));
          Store_field(ret, 1, TranslateExpr(x->getExpr()));
        });

      GENERATE_NODE_INDIRECT(CXXDeleteExpr, ret, node, 6, {
          Store_field(ret, 0, TranslateExpr(x->getArgument()));
          Store_field_option(ret, 1, x->getOperatorDelete(), TranslateFunctionDecl(x->getOperatorDelete()));
          Store_field(ret, 2, TranslateQualTypeOption(x->getDestroyedType()));
          Store_field(ret, 3, Val_bool(x->isGlobalDelete()));
          Store_field(ret, 4, Val_bool(x->isArrayForm()));
          Store_field(ret, 5, Val_bool(x->doesUsualArrayDeleteWantSize()));
        });

      GENERATE_NODE_INDIRECT(CXXDependentScopeMemberExpr, ret, node, 7, {
          Store_field_option(ret, 0, !x->isImplicitAccess(), TranslateExpr(x->getBase()));
          Store_field(ret, 1, TranslateQualType(x->getBaseType()));
          Store_field(ret, 2, Val_bool(x->isArrow()));
          Store_field(ret, 3, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
          Store_field_option(ret, 4, x->getFirstQualifierFoundInScope(), TranslateDecl(x->getFirstQualifierFoundInScope()));
          Store_field(ret, 5, TranslateDeclarationName(x->getMember()));
          const TemplateArgumentLoc* tmp = x->getTemplateArgs();
          Store_field_array(ret, 6, x->getNumTemplateArgs(), TranslateTemplateArgumentLoc(tmp[i]));
        });

      GENERATE_NODE_INDIRECT(CXXFoldExpr, ret, node, 4, {
          Store_field(ret, 0, TranslateExpr(x->getPattern()));
          Store_field_option(ret, 1, x->getInit(), TranslateExpr(x->getInit()));
          Store_field(ret, 2, Val_bool(x->isRightFold()));
          Store_field(ret, 3, TranslateBinaryOperatorKind(x->getOperator(), x));
       });

      GENERATE_NODE_INDIRECT(CXXInheritedCtorInitExpr, ret, node, 4, {
          Store_field(ret, 0, TranslateFunctionDecl(x->getConstructor()));
          Store_field(ret, 1, TranslateConstructionKind(x->getConstructionKind(), x));
          Store_field(ret, 2, Val_bool(x->constructsVBase()));
          Store_field(ret, 3, Val_bool(x->inheritedFromVBase()));
        });

      GENERATE_NODE_INDIRECT(CXXNewExpr, ret, node, 11, {
          Store_field(ret, 0, TranslateQualType(x->getAllocatedType()));
          Store_field_option(ret, 1, x->getOperatorNew(), TranslateFunctionDecl(x->getOperatorNew()));
          Store_field_option(ret, 2, x->getOperatorDelete(), TranslateFunctionDecl(x->getOperatorDelete()));
#if CLANG_VERSION_MAJOR >= 16
          Store_field_option(ret, 3, x->isArray() && x->getArraySize().has_value(), TranslateExpr(x->getArraySize().value()));
#elif CLANG_VERSION_MAJOR >= 9
          Store_field_option(ret, 3, x->isArray() && x->getArraySize().hasValue(), TranslateExpr(x->getArraySize().getValue()));
#else
          Store_field_option(ret, 3, x->isArray(), TranslateExpr(x->getArraySize()));
#endif
          Store_field(ret, 4, Val_bool(x->isGlobalNew()));
          int r = 0;
          switch (x->getInitializationStyle()) {
#if CLANG_VERSION_MAJOR < 18
            GENERATE_CASE_PREFIX(r, CXXNewExpr::, New_, NoInit);
            GENERATE_CASE_PREFIX(r, CXXNewExpr::, New_, CallInit);
            GENERATE_CASE_PREFIX(r, CXXNewExpr::, New_, ListInit);
#else
            GENERATE_CASE_PREFIX_ALT(r, CXXNewInitializationStyle::, New_, None, NoInit);
            GENERATE_CASE_PREFIX_ALT(r, CXXNewInitializationStyle::, New_, Parens, CallInit);
            GENERATE_CASE_PREFIX_ALT(r, CXXNewInitializationStyle::, New_, Braces, ListInit);
#endif
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown initialization style: " << static_cast<int>(x->getInitializationStyle()) << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown initialization style");
          }
          Store_field(ret, 5, Val_int(r));
          Store_field_option(ret, 6, x->hasInitializer(), TranslateExpr(x->getInitializer()));
          Store_field_array(ret, 7, x->getNumPlacementArgs(), TranslateExpr(x->getPlacementArg(i)));
          Store_field_option(ret, 8, x->getConstructExpr(), TranslateCXXConstructExpr(x->getConstructExpr()));
          Store_field(ret, 9, Val_bool(x->passAlignment()));
          Store_field(ret, 10, Val_bool(x->doesUsualArrayDeleteWantSize()));

        });

      GENERATE_NODE(CXXNoexceptExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getOperand()));
          Store_field(ret, 1, Val_bool(x->getValue()));

        });

      GENERATE_NODE_CONSTANT(CXXNullPtrLiteralExpr, ret, node);

      GENERATE_NODE_INDIRECT(CXXPseudoDestructorExpr, ret, node, 4, {
          Store_field(ret, 0, TranslateExpr(x->getBase()));
          Store_field_option(ret, 1, x->hasQualifier(), TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
          Store_field(ret, 2, Val_bool(x->isArrow()));
          Store_field(ret, 3, TranslateQualType(x->getDestroyedType()));
        });


      GENERATE_NODE_CONSTANT(CXXScalarValueInitExpr, ret, node);

      GENERATE_NODE(CXXStdInitializerListExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE(CXXThisExpr, ret, node, 1, {
          Store_field(ret, 0, Val_bool(x->isImplicit()));
        });

      GENERATE_NODE_INDIRECT(CXXThrowExpr, ret, node, 2, {
          Store_field_option(ret, 0, x->getSubExpr(), TranslateExpr(x->getSubExpr()));
          Store_field(ret, 1, Val_bool(x->isThrownVariableInScope()));

        });

      GENERATE_NODE_INDIRECT(CXXTypeidExpr, ret, node, 3, {
          Store_field_option(ret, 0, x->isTypeOperand(), TranslateQualType(x->getTypeOperand(*Context)));
          Store_field_option(ret, 1, !x->isTypeOperand(), TranslateExpr(x->getExprOperand()));
          Store_field(ret, 2, Val_bool(x->isPotentiallyEvaluated()));
        });

      GENERATE_NODE_INDIRECT(CXXUnresolvedConstructExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateQualType(x->getTypeAsWritten()));
#if CLANG_VERSION_MAJOR >= 12
          Store_field_array(ret, 1, x->getNumArgs(), TranslateExpr(x->getArg(i)));
#else
          Store_field_array(ret, 1, x->arg_size(), TranslateExpr(x->getArg(i)));
#endif
        });

      GENERATE_NODE_INDIRECT(DependentScopeDeclRefExpr, ret, node, 3, {
          Store_field(ret, 0, TranslateDeclarationName(x->getDeclName()));
          Store_field(ret, 1, TranslateNestedNameSpecifierLoc(x->getQualifierLoc()));
          const TemplateArgumentLoc* tmp = x->getTemplateArgs();
          Store_field_array(ret, 2, x->getNumTemplateArgs(), TranslateTemplateArgumentLoc(tmp[i]));

        });

      GENERATE_NODE_INDIRECT(ExpressionTraitExpr, ret, node, 3, {
          int r = 0;
          switch (x->getTrait()) {
            GENERATE_CASE(r, ET_IsLValueExpr);
            GENERATE_CASE(r, ET_IsRValueExpr);
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown expression trait: " << x->getTrait() << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown expression trait");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field(ret, 1, TranslateExpr(x->getQueriedExpression()));
          Store_field(ret, 2, Val_bool(x->getValue()));
        });

      GENERATE_NODE_INDIRECT(ExprWithCleanups, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
#if CLANG_VERSION_MAJOR < 11
          Store_field_array(ret, 1, x->getNumObjects(), TranslateBlockDecl(x->getObject(i)));
#else
          // TODO: API for cleanup has changed for recent Clang; we need to support CleanupObject
          caml_failwith("mlCLangAST: ExprWithCleanups not supported");
#endif
        });

      GENERATE_NODE(FunctionParmPackExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateVarDecl(x->getParameterPack()));
          Store_field_array(ret, 1, x->getNumExpansions(), TranslateVarDecl(x->getExpansion(i)));
        });

      GENERATE_NODE_INDIRECT(MaterializeTemporaryExpr, ret, node, 4, {
#if CLANG_VERSION_MAJOR < 10
          Store_field(ret, 0, TranslateExpr(x->GetTemporaryExpr()));
#else
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
#endif
          Store_field(ret, 1, TranslateStorageDuration(x->getStorageDuration()));
          Store_field_option(ret, 2, x->getExtendingDecl(), TranslateDecl(x->getExtendingDecl()));
          Store_field(ret, 3, Val_bool(x->isBoundToLvalueReference()));
        });

      GENERATE_NODE_INDIRECT(PackExpansionExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getPattern()));
          const Optional<unsigned> num = x->getNumExpansions();
#if CLANG_VERSION_MAJOR >= 16
          Store_field_option(ret, 1, num.has_value(), Val_int(num.value()));
#else
          Store_field_option(ret, 1, num.hasValue(), Val_int(num.getValue()));
#endif
        });

      GENERATE_NODE_INDIRECT(SizeOfPackExpr, ret, node, 4, {
          Store_field(ret, 0, TranslateDecl(x->getPack()));
          Store_field_option(ret, 1, !x->isValueDependent(), Val_int(x->getPackLength()));
          Store_field(ret, 2, Val_bool(x->isPartiallySubstituted()));
          if (x->isPartiallySubstituted()) {
            const ArrayRef<TemplateArgument> a = x->getPartialArguments();
            Store_field_array(ret, 3, a.size(), TranslateTemplateArgument(a[i]));
          }
          else {
            Store_field(ret, 3, Atom(0));
          }
        });

      GENERATE_NODE_INDIRECT(SubstNonTypeTemplateParmExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getReplacement()));
          Store_field(ret, 1, TranslateNonTypeTemplateParmDecl(x->getParameter()));
        });

      GENERATE_NODE_INDIRECT(SubstNonTypeTemplateParmPackExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateNonTypeTemplateParmDecl(x->getParameterPack()));
          Store_field(ret, 1, TranslateTemplateArgument(x->getArgumentPack()));

        });

      GENERATE_NODE_INDIRECT(TypeTraitExpr, ret, node, 3, {
          Store_field(ret, 0, TranslateTypeTrait(x->getTrait(), node));
          Store_field_option(ret, 1, !x->isValueDependent(), Val_bool(x->getValue()));
          const ArrayRef<TypeSourceInfo*> a = x->getArgs();
          Store_field_array(ret, 2, x->getNumArgs(), TranslateQualType(a[i]->getType()));
        });

      GENERATE_NODE_INDIRECT(UnresolvedLookupExpr, ret, node, 6, {
          const ArrayRef<TemplateArgumentLoc> a = x->template_arguments();
          Store_field(ret, 0, Val_bool(x->requiresADL()));
          Store_field(ret, 1, Val_bool(x->isOverloaded()));
          Store_field_option(ret, 2, x->getNamingClass(), TranslateRecordDecl(x->getNamingClass()));
          Store_field(ret, 3, TranslateDeclarationName(x->getName()));
          Store_field_list(ret, 4, x->decls(), TranslateDecl(child));
          Store_field_array(ret, 5, a.size(), TranslateTemplateArgumentLoc(a[i]));
        });

      GENERATE_NODE_INDIRECT(UnresolvedMemberExpr, ret, node, 9, {
          const ArrayRef<TemplateArgumentLoc> a = x->template_arguments();
          Store_field_option(ret, 0, !x->isImplicitAccess(), TranslateExpr(x->getBase()));
          Store_field(ret, 1, TranslateQualType(x->getBaseType()));
          Store_field(ret, 2, Val_bool(x->isImplicitAccess()));
          Store_field(ret, 3, Val_bool(x->isArrow()));
          Store_field_option(ret, 4, x->getNamingClass(), TranslateRecordDecl(x->getNamingClass()));
          Store_field(ret, 5, TranslateDeclarationName(x->getMemberName()));
          Store_field(ret, 6, TranslateDeclarationName(x->getName()));
          Store_field_list(ret, 7, x->decls(), TranslateDecl(child));
          Store_field_array(ret, 8, a.size(), TranslateTemplateArgumentLoc(a[i]));
       });

      GENERATE_NODE_INDIRECT(LambdaExpr, ret, node, 11, {
          int r = 0;
          switch (x->getCaptureDefault()) {
            GENERATE_CASE(r, LCD_None);
            GENERATE_CASE(r, LCD_ByCopy);
            GENERATE_CASE(r, LCD_ByRef);
          default:
            if (verbose_exn) { node->dump(); std::cout << "unknown lambda capture default: " << x->getCaptureDefault() << DEBUG_SOURCE_RANGE(node) << std::endl;}
            caml_failwith("mlClangAST: unknown lambda capture default");
          }
          Store_field(ret, 0, Val_int(r));
          Store_field_list(ret, 1, x->captures(), TranslateLambdaCapture(child));
          Store_field_list(ret, 2, x->capture_inits(), TranslateExpr(child));
          Store_field(ret, 3, TranslateRecordDecl(x->getLambdaClass()));
          Store_field(ret, 4, TranslateFunctionDecl(x->getCallOperator()));
          Store_field_option(ret, 5, x->getTemplateParameterList(), TranslateTemplateParameterList(x->getTemplateParameterList()));
          Store_field(ret, 6, Val_bool(x->isGenericLambda()));
          Store_field(ret, 7, TranslateStmt(x->getBody()));
          Store_field(ret, 8, Val_bool(x->isMutable()));
          Store_field(ret, 9, Val_bool(x->hasExplicitParameters()));
          Store_field(ret, 10, Val_bool(x->hasExplicitResultType()));
        });

      /* Vectors */

      GENERATE_NODE(ConvertVectorExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSrcExpr()));
        });

      GENERATE_NODE(ExtVectorElementExpr, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getBase()));
          Store_field(ret, 1, caml_copy_string(x->getAccessor().getNameStart()));
        });

      GENERATE_NODE(ShuffleVectorExpr, ret, node, 1, {
          Store_field_array(ret, 0, x->getNumSubExprs(), TranslateExpr(x->getExpr(i)));
        });


      /* Clang >= 8 */

#if CLANG_VERSION_MAJOR >= 8
        
      GENERATE_NODE(ConstantExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

      GENERATE_NODE(FullExpr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x->getSubExpr()));
        });

#endif

      // Default
      if (ret == Val_int(-1)) {
        ret = caml_alloc(2, MLTAG_UnknownExpr);
        Store_field(ret, 0, Val_int(node->getStmtClass()));
        Store_field(ret, 1, caml_copy_string(node->getStmtClassName()));
        if (log_unknown) { std::cerr << "mlClangAST: unhandled Expr node encountered: " << node->getStmtClassName() << std::endl; }

      }

      Store_field(ret2, 0, ret);
      Store_field(ret2, 1, TranslateQualTypeOption(node->getType()));
      Store_field(ret2, 2, loc.TranslateSourceRange(node->getSourceRange()));
    });

  if (verbose_alloc)
    std::cout << "end TranslateExpr " << std::hex << node << std::dec << " " << node->getStmtClassName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  CAMLreturn(ret2);
}



/* type_trait */
enum {
  MLTAG_UTT_HasNothrowAssign,
  MLTAG_UTT_HasNothrowMoveAssign,
  MLTAG_UTT_HasNothrowCopy,
  MLTAG_UTT_HasNothrowConstructor,
  MLTAG_UTT_HasTrivialAssign,
  MLTAG_UTT_HasTrivialMoveAssign,
  MLTAG_UTT_HasTrivialCopy,
  MLTAG_UTT_HasTrivialDefaultConstructor,
  MLTAG_UTT_HasTrivialMoveConstructor,
  MLTAG_UTT_HasTrivialDestructor,
  MLTAG_UTT_HasVirtualDestructor,
  MLTAG_UTT_IsAbstract,
  MLTAG_UTT_IsArithmetic,
  MLTAG_UTT_IsArray,
  MLTAG_UTT_IsClass,
  MLTAG_UTT_IsCompleteType,
  MLTAG_UTT_IsCompound,
  MLTAG_UTT_IsConst,
  MLTAG_UTT_IsDestructible,
  MLTAG_UTT_IsEmpty,
  MLTAG_UTT_IsEnum,
  MLTAG_UTT_IsFinal,
  MLTAG_UTT_IsFloatingPoint,
  MLTAG_UTT_IsFunction,
  MLTAG_UTT_IsFundamental,
  MLTAG_UTT_IsIntegral,
  MLTAG_UTT_IsInterfaceClass,
  MLTAG_UTT_IsLiteral,
  MLTAG_UTT_IsLvalueReference,
  MLTAG_UTT_IsMemberFunctionPointer,
  MLTAG_UTT_IsMemberObjectPointer,
  MLTAG_UTT_IsMemberPointer,
  MLTAG_UTT_IsNothrowDestructible,
  MLTAG_UTT_IsObject,
  MLTAG_UTT_IsPOD,
  MLTAG_UTT_IsPointer,
  MLTAG_UTT_IsPolymorphic,
  MLTAG_UTT_IsReference,
  MLTAG_UTT_IsRvalueReference,
  MLTAG_UTT_IsScalar,
  MLTAG_UTT_IsSealed,
  MLTAG_UTT_IsSigned,
  MLTAG_UTT_IsStandardLayout,
  MLTAG_UTT_IsTrivial,
  MLTAG_UTT_IsTriviallyCopyable,
  MLTAG_UTT_IsUnion,
  MLTAG_UTT_IsUnsigned,
  MLTAG_UTT_IsVoid,
  MLTAG_UTT_IsVolatile,
  MLTAG_BTT_IsBaseOf,
  MLTAG_BTT_IsConvertible,
  MLTAG_BTT_IsConvertibleTo,
  MLTAG_BTT_IsSame,
  MLTAG_BTT_TypeCompatible,
  MLTAG_BTT_IsAssignable,
  MLTAG_BTT_IsNothrowAssignable,
  MLTAG_BTT_IsTriviallyAssignable,
  MLTAG_TT_IsConstructible,
  MLTAG_TT_IsNothrowConstructible,
  MLTAG_TT_IsTriviallyConstructible,
};

/* TypeTrait -> type_trait */
CAMLprim value MLTreeBuilderVisitor::TranslateTypeTrait(TypeTrait trait, const Expr * node) {
  int r = 0;
  switch (trait) {
    GENERATE_CASE(r, UTT_HasNothrowAssign);
    GENERATE_CASE(r, UTT_HasNothrowMoveAssign);
    GENERATE_CASE(r, UTT_HasNothrowCopy);
    GENERATE_CASE(r, UTT_HasNothrowConstructor);
    GENERATE_CASE(r, UTT_HasTrivialAssign);
    GENERATE_CASE(r, UTT_HasTrivialMoveAssign);
    GENERATE_CASE(r, UTT_HasTrivialCopy);
    GENERATE_CASE(r, UTT_HasTrivialDefaultConstructor);
    GENERATE_CASE(r, UTT_HasTrivialMoveConstructor);
    GENERATE_CASE(r, UTT_HasTrivialDestructor);
    GENERATE_CASE(r, UTT_HasVirtualDestructor);
    GENERATE_CASE(r, UTT_IsAbstract);
    GENERATE_CASE(r, UTT_IsArithmetic);
    GENERATE_CASE(r, UTT_IsArray);
    GENERATE_CASE(r, UTT_IsClass);
    GENERATE_CASE(r, UTT_IsCompleteType);
    GENERATE_CASE(r, UTT_IsCompound);
    GENERATE_CASE(r, UTT_IsConst);
    GENERATE_CASE(r, UTT_IsDestructible);
    GENERATE_CASE(r, UTT_IsEmpty);
    GENERATE_CASE(r, UTT_IsEnum);
    GENERATE_CASE(r, UTT_IsFinal);
    GENERATE_CASE(r, UTT_IsFloatingPoint);
    GENERATE_CASE(r, UTT_IsFunction);
    GENERATE_CASE(r, UTT_IsFundamental);
    GENERATE_CASE(r, UTT_IsIntegral);
    GENERATE_CASE(r, UTT_IsInterfaceClass);
    GENERATE_CASE(r, UTT_IsLiteral);
    GENERATE_CASE(r, UTT_IsLvalueReference);
    GENERATE_CASE(r, UTT_IsMemberFunctionPointer);
    GENERATE_CASE(r, UTT_IsMemberObjectPointer);
    GENERATE_CASE(r, UTT_IsMemberPointer);
    GENERATE_CASE(r, UTT_IsNothrowDestructible);
    GENERATE_CASE(r, UTT_IsObject);
    GENERATE_CASE(r, UTT_IsPOD);
    GENERATE_CASE(r, UTT_IsPointer);
    GENERATE_CASE(r, UTT_IsPolymorphic);
    GENERATE_CASE(r, UTT_IsReference);
    GENERATE_CASE(r, UTT_IsRvalueReference);
    GENERATE_CASE(r, UTT_IsScalar);
    GENERATE_CASE(r, UTT_IsSealed);
    GENERATE_CASE(r, UTT_IsSigned);
    GENERATE_CASE(r, UTT_IsStandardLayout);
    GENERATE_CASE(r, UTT_IsTrivial);
    GENERATE_CASE(r, UTT_IsTriviallyCopyable);
    GENERATE_CASE(r, UTT_IsUnion);
    GENERATE_CASE(r, UTT_IsUnsigned);
    GENERATE_CASE(r, UTT_IsVoid);
    GENERATE_CASE(r, UTT_IsVolatile);
    GENERATE_CASE(r, BTT_IsBaseOf);
    GENERATE_CASE(r, BTT_IsConvertible);
    GENERATE_CASE(r, BTT_IsConvertibleTo);
    GENERATE_CASE(r, BTT_IsSame);
    GENERATE_CASE(r, BTT_TypeCompatible);
    GENERATE_CASE(r, BTT_IsAssignable);
    GENERATE_CASE(r, BTT_IsNothrowAssignable);
    GENERATE_CASE(r, BTT_IsTriviallyAssignable);
    GENERATE_CASE(r, TT_IsConstructible);
    GENERATE_CASE(r, TT_IsNothrowConstructible);
    GENERATE_CASE(r, TT_IsTriviallyConstructible);
  default:
    if (verbose_exn) { node->dump(); std::cout << "unknown type trait: " << trait << DEBUG_SOURCE_RANGE(node) << std::endl;}
    caml_failwith("mlClangAST: unknown type trait");
  }
  return Val_int(r);
}


CAMLprim value MLTreeBuilderVisitor::TranslateCXXConstructExpr(const CXXConstructExpr * x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TranslateCXXConstructExpr");
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 5, {
      Store_field(ret, 0, TranslateFunctionDecl(x->getConstructor()));
      Store_field(ret, 1, TranslateConstructionKind(x->getConstructionKind(), x));
      Store_field_array(ret, 2, x->getNumArgs(), TranslateExpr(x->getArg(i)));
      Store_field(ret, 3, Val_bool(x->requiresZeroInitialization()));
      Store_field(ret, 4, Val_bool(isa<CXXTemporaryObjectExpr>(x)));
   });

  CAMLreturn(ret);
}


/* Expr -> Z.t option (if constant) */
CAMLprim value MLTreeBuilderVisitor::TranslateConstantIntegerExpr(const Expr * node) {
  CAMLparam0();
  CAMLlocal1(ret);
  check_null(node, "TranslateConstantIntegerExpr");
#if CLANG_VERSION_MAJOR <= 7
  llvm::APSInt r;
  if (node->EvaluateAsInt(r,*Context)) {
    ret = caml_alloc_tuple(1);
    Store_field(ret, 0, TranslateAPSInt(r));
  }
#else
  Expr::EvalResult r;
  if (node->EvaluateAsInt(r,*Context) && r.Val.isInt()) {
    ret = caml_alloc_tuple(1);
    Store_field(ret, 0, TranslateAPSInt(r.Val.getInt()));
  }
#endif
  else {
    ret = Val_false;
  }
  CAMLreturn(ret);
}

// designator
enum {
  MLTAG_Designator_Field,
  MLTAG_Designator_Array,
  MLTAG_Designator_ArrayRange,
};

/* DesignatedInitExpr::Designator -> designator */
CAMLprim value MLTreeBuilderVisitor::TranslateDesignator(const DesignatedInitExpr* x, const DesignatedInitExpr::Designator* d) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x && d, "TranslateDesignator");

  if (d->isFieldDesignator()) {
    ret = caml_alloc(1, MLTAG_Designator_Field);
#if CLANG_VERSION_MAJOR >= 17
    Store_field(ret, 0, TranslateFieldDecl(d->getFieldDecl()));
#else
    Store_field(ret, 0, TranslateFieldDecl(d->getField()));
#endif
 }
  else if (d->isArrayDesignator()) {
    ret = caml_alloc(1, MLTAG_Designator_Array);
    Store_field(ret, 0, TranslateExpr(x->getArrayIndex(*d)));
  }
  else if (d->isArrayRangeDesignator()) {
    ret = caml_alloc(2, MLTAG_Designator_ArrayRange);
    Store_field(ret, 0, TranslateExpr(x->getArrayRangeStart(*d)));
    Store_field(ret, 1, TranslateExpr(x->getArrayRangeEnd(*d)));
  }
  else {
    if (verbose_exn) x->dump();
    caml_failwith("mlClangAST: unknown designator kind in designated init expression");
  }

  CAMLreturn(ret);
}

// offsetof_node
enum {
  MLTAG_Offsetof_Array,
  MLTAG_Offsetof_Field,
  MLTAG_Offsetof_Identifier,
};

/* OffsetOfNode -> offsetof_node */
CAMLprim value MLTreeBuilderVisitor::TranslateOffsetOfNode(const OffsetOfExpr * x, const OffsetOfNode & n) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TranslateOffsetOfNode");

  switch (n.getKind()) {
  case OffsetOfNode::Array:
    ret = caml_alloc(1, MLTAG_Offsetof_Array);
    Store_field(ret, 0, TranslateExpr(x->getIndexExpr(n.getArrayExprIndex())));
    break;
  case OffsetOfNode::Field:
    ret = caml_alloc(1, MLTAG_Offsetof_Field);
    Store_field(ret, 0, TranslateFieldDecl(n.getField()));
    break;
  case OffsetOfNode::Identifier:
    ret = caml_alloc(1, MLTAG_Offsetof_Identifier);
    Store_field(ret, 0, caml_copy_string(n.getFieldName()->getName().str().c_str()));
    break;
  default:
    if (verbose_exn) { x->dump(); std::cout << "unknown OffsetOfNode kind: " << DEBUG_SOURCE_RANGE(x) << n.getKind() << std::endl; }
    caml_failwith("mlClangAST: unknown OffsetOfNode kind");
  }

  CAMLreturn(ret);
}


/* OpaqueValueExpr -> opaque_expr */
CAMLprim value MLTreeBuilderVisitor::TranslateOpaqueValueExpr(const OpaqueValueExpr * node) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(node, "TranslateOpaqueValueExpr");
  WITH_CACHE_TUPLE(cacheMisc, ret, node, 3, {
      Store_field_option(ret, 0, node->getSourceExpr(), TranslateExpr(node->getSourceExpr()));
      Store_field(ret, 1, TranslateQualType(node->getType()));
      Store_field(ret, 2, loc.TranslateSourceRange(node->getSourceRange()));
    });

  CAMLreturn(ret);
}


/* unary_operator */
enum {
  MLTAG_UO_PostInc,
  MLTAG_UO_PostDec,
  MLTAG_UO_PreInc,
  MLTAG_UO_PreDec,
  MLTAG_UO_AddrOf,
  MLTAG_UO_Deref,
  MLTAG_UO_Plus,
  MLTAG_UO_Minus,
  MLTAG_UO_Not,
  MLTAG_UO_LNot,
  MLTAG_UO_Real,
  MLTAG_UO_Imag,
  MLTAG_UO_Extension,
  MLTAG_UO_Coawait,
};

/* UnaryOperatorKind -> unary_operator */
CAMLprim value MLTreeBuilderVisitor::TranslateUnaryOperatorKind(UnaryOperatorKind k, const Expr * x) {
  CAMLparam0();
  int r = 0;
  switch(k) {
    GENERATE_CASE(r, UO_PostInc);
    GENERATE_CASE(r, UO_PostDec);
    GENERATE_CASE(r, UO_PreInc);
    GENERATE_CASE(r, UO_PreDec);
    GENERATE_CASE(r, UO_AddrOf);
    GENERATE_CASE(r, UO_Deref);
    GENERATE_CASE(r, UO_Plus);
    GENERATE_CASE(r, UO_Minus);
    GENERATE_CASE(r, UO_Not);
    GENERATE_CASE(r, UO_LNot);
    GENERATE_CASE(r, UO_Real);
    GENERATE_CASE(r, UO_Imag);
    GENERATE_CASE(r, UO_Extension);
    GENERATE_CASE(r, UO_Coawait);
  default:
    if (verbose_exn) { if (x) x->dump(); std::cout << "unknown unary operator: " << k << DEBUG_SOURCE_RANGE(x) << std::endl; }
    caml_failwith("mlClangAST: unknown unary operator kind");
  }
  CAMLreturn(Val_int(r));
}

/* binary_operator */
enum {
  MLTAG_BO_Mul,
  MLTAG_BO_Div,
  MLTAG_BO_Rem,
  MLTAG_BO_Add,
  MLTAG_BO_Sub,
  MLTAG_BO_Shl,
  MLTAG_BO_Shr,
  MLTAG_BO_LT,
  MLTAG_BO_GT,
  MLTAG_BO_LE,
  MLTAG_BO_GE,
  MLTAG_BO_EQ,
  MLTAG_BO_NE,
  MLTAG_BO_And,
  MLTAG_BO_Xor,
  MLTAG_BO_Or,
  MLTAG_BO_LAnd,
  MLTAG_BO_LOr,
  MLTAG_BO_Comma,
  MLTAG_BO_Assign,
  MLTAG_BO_PtrMemD,
  MLTAG_BO_PtrMemI,
};

/* BinaryOperatorKind -> binary_operator */
CAMLprim value MLTreeBuilderVisitor::TranslateBinaryOperatorKind(BinaryOperatorKind k, const Expr * x) {
  CAMLparam0();
  int r = 0;
  switch(k) {
    GENERATE_CASE(r, BO_Mul);
    GENERATE_CASE(r, BO_Div);
    GENERATE_CASE(r, BO_Rem);
    GENERATE_CASE(r, BO_Add);
    GENERATE_CASE(r, BO_Sub);
    GENERATE_CASE(r, BO_Shl);
    GENERATE_CASE(r, BO_Shr);
    GENERATE_CASE(r, BO_LT);
    GENERATE_CASE(r, BO_GT);
    GENERATE_CASE(r, BO_LE);
    GENERATE_CASE(r, BO_GE);
    GENERATE_CASE(r, BO_EQ);
    GENERATE_CASE(r, BO_NE);
    GENERATE_CASE(r, BO_And);
    GENERATE_CASE(r, BO_Xor);
    GENERATE_CASE(r, BO_Or);
    GENERATE_CASE(r, BO_LAnd);
    GENERATE_CASE(r, BO_LOr);
    GENERATE_CASE(r, BO_Comma);
    GENERATE_CASE(r, BO_Assign);
    GENERATE_CASE(r, BO_PtrMemD);
    GENERATE_CASE(r, BO_PtrMemI);
  default:
    if (verbose_exn) { if (x) x->dump(); std::cout << "unknown binary operator: " << k << DEBUG_SOURCE_RANGE(x) << std::endl; }
    caml_failwith("mlClangAST: unknown binary operator kind");
  }
  CAMLreturn(Val_int(r));
}


/* compound_assign_operator */
enum {
  MLTAG_BO_MulAssign,
  MLTAG_BO_DivAssign,
  MLTAG_BO_RemAssign,
  MLTAG_BO_AddAssign,
  MLTAG_BO_SubAssign,
  MLTAG_BO_ShlAssign,
  MLTAG_BO_ShrAssign,
  MLTAG_BO_AndAssign,
  MLTAG_BO_XorAssign,
  MLTAG_BO_OrAssign,
};

/* BinaryOperatorKind -> compound_assign_operator */
CAMLprim value MLTreeBuilderVisitor::TranslateCompoundAssignOperatorKind(BinaryOperatorKind k, const Expr * x) {
  CAMLparam0();
  int r = 0;
  switch(k) {
    GENERATE_CASE(r, BO_MulAssign);
    GENERATE_CASE(r, BO_DivAssign);
    GENERATE_CASE(r, BO_RemAssign);
    GENERATE_CASE(r, BO_AddAssign);
    GENERATE_CASE(r, BO_SubAssign);
    GENERATE_CASE(r, BO_ShlAssign);
    GENERATE_CASE(r, BO_ShrAssign);
    GENERATE_CASE(r, BO_AndAssign);
    GENERATE_CASE(r, BO_XorAssign);
    GENERATE_CASE(r, BO_OrAssign);
  default:
    if (verbose_exn) { if (x) x->dump(); std::cout << "unknown compound assignment operator: " << k << DEBUG_SOURCE_RANGE(x) << std::endl; }
    caml_failwith("mlClangAST: unknown compound assignment operator kind");
  }
  CAMLreturn(Val_int(r));
}

/* overloaded_operator */
enum {
  MLTAG_OO_New,
  MLTAG_OO_Delete,
  MLTAG_OO_Array_New,
  MLTAG_OO_Array_Delete,
  MLTAG_OO_Plus,
  MLTAG_OO_Minus,
  MLTAG_OO_Star,
  MLTAG_OO_Slash,
  MLTAG_OO_Percent,
  MLTAG_OO_Caret,
  MLTAG_OO_Amp,
  MLTAG_OO_Pipe,
  MLTAG_OO_Tilde,
  MLTAG_OO_Exclaim,
  MLTAG_OO_Equal,
  MLTAG_OO_Less,
  MLTAG_OO_Greater,
  MLTAG_OO_PlusEqual,
  MLTAG_OO_MinusEqual,
  MLTAG_OO_StarEqual,
  MLTAG_OO_SlashEqual,
  MLTAG_OO_PercentEqual,
  MLTAG_OO_CaretEqual,
  MLTAG_OO_AmpEqual,
  MLTAG_OO_PipeEqual,
  MLTAG_OO_LessLess,
  MLTAG_OO_GreaterGreater,
  MLTAG_OO_LessLessEqual,
  MLTAG_OO_GreaterGreaterEqual,
  MLTAG_OO_EqualEqual,
  MLTAG_OO_ExclaimEqual,
  MLTAG_OO_LessEqual,
  MLTAG_OO_GreaterEqual,
  MLTAG_OO_AmpAmp,
  MLTAG_OO_PipePipe,
  MLTAG_OO_PlusPlus,
  MLTAG_OO_MinusMinus,
  MLTAG_OO_Comma,
  MLTAG_OO_ArrowStar,
  MLTAG_OO_Arrow,
  MLTAG_OO_Call,
  MLTAG_OO_Subscript,
  MLTAG_OO_Conditional,
  MLTAG_OO_Coawait,
};

/* OverloadedOperatorKind -> overloaded_operator */
CAMLprim value MLTreeBuilderVisitor::TranslateOverloadedOperatorKind(OverloadedOperatorKind k, const Expr * x) {
  CAMLparam0();
  int r = 0;
  switch(k) {
    GENERATE_CASE(r, OO_New);
    GENERATE_CASE(r, OO_Delete);
    GENERATE_CASE(r, OO_Array_New);
    GENERATE_CASE(r, OO_Array_Delete);
    GENERATE_CASE(r, OO_Plus);
    GENERATE_CASE(r, OO_Minus);
    GENERATE_CASE(r, OO_Star);
    GENERATE_CASE(r, OO_Slash);
    GENERATE_CASE(r, OO_Percent);
    GENERATE_CASE(r, OO_Caret);
    GENERATE_CASE(r, OO_Amp);
    GENERATE_CASE(r, OO_Pipe);
    GENERATE_CASE(r, OO_Tilde);
    GENERATE_CASE(r, OO_Exclaim);
    GENERATE_CASE(r, OO_Equal);
    GENERATE_CASE(r, OO_Less);
    GENERATE_CASE(r, OO_Greater);
    GENERATE_CASE(r, OO_PlusEqual);
    GENERATE_CASE(r, OO_MinusEqual);
    GENERATE_CASE(r, OO_StarEqual);
    GENERATE_CASE(r, OO_SlashEqual);
    GENERATE_CASE(r, OO_PercentEqual);
    GENERATE_CASE(r, OO_CaretEqual);
    GENERATE_CASE(r, OO_AmpEqual);
    GENERATE_CASE(r, OO_PipeEqual);
    GENERATE_CASE(r, OO_LessLess);
    GENERATE_CASE(r, OO_GreaterGreater);
    GENERATE_CASE(r, OO_LessLessEqual);
    GENERATE_CASE(r, OO_GreaterGreaterEqual);
    GENERATE_CASE(r, OO_EqualEqual);
    GENERATE_CASE(r, OO_ExclaimEqual);
    GENERATE_CASE(r, OO_LessEqual);
    GENERATE_CASE(r, OO_GreaterEqual);
    GENERATE_CASE(r, OO_AmpAmp);
    GENERATE_CASE(r, OO_PipePipe);
    GENERATE_CASE(r, OO_PlusPlus);
    GENERATE_CASE(r, OO_MinusMinus);
    GENERATE_CASE(r, OO_Comma);
    GENERATE_CASE(r, OO_ArrowStar);
    GENERATE_CASE(r, OO_Arrow);
    GENERATE_CASE(r, OO_Call);
    GENERATE_CASE(r, OO_Subscript);
    GENERATE_CASE(r, OO_Conditional);
    GENERATE_CASE(r, OO_Coawait);
  default:
    if (verbose_exn) { if (x) x->dump(); std::cout << "unknown overloaded operator: " << k << DEBUG_SOURCE_RANGE(x) << std::endl; }
    caml_failwith("mlClangAST: unknown overloaded operator");
  }
  CAMLreturn(Val_int(r));
}


/* construction_kind */
enum {
  MLTAG_CK_Complete,
  MLTAG_CK_NonVirtualBase,
  MLTAG_CK_VirtualBase,
  MLTAG_CK_Delegating,
};

/* CXXConstructExpr::ConstructionKind -> construction_kind */
#if CLANG_VERSION_MAJOR < 18
CAMLprim value MLTreeBuilderVisitor::TranslateConstructionKind(CXXConstructExpr::ConstructionKind k, const Expr * x) {
#else
CAMLprim value MLTreeBuilderVisitor::TranslateConstructionKind(CXXConstructionKind k, const Expr * x) {
#endif
  CAMLparam0();
  int r = 0;
  switch(k) {
#if CLANG_VERSION_MAJOR < 18
    GENERATE_CASE_PREFIX(r, CXXConstructExpr::, , CK_Complete);
    GENERATE_CASE_PREFIX(r, CXXConstructExpr::, , CK_NonVirtualBase);
    GENERATE_CASE_PREFIX(r, CXXConstructExpr::, , CK_VirtualBase);
    GENERATE_CASE_PREFIX(r, CXXConstructExpr::, , CK_Delegating);
#else
    GENERATE_CASE_PREFIX_ALT(r, CXXConstructionKind::, , Complete, CK_Complete);
    GENERATE_CASE_PREFIX_ALT(r, CXXConstructionKind::, , NonVirtualBase, CK_NonVirtualBase);
    GENERATE_CASE_PREFIX_ALT(r, CXXConstructionKind::, , VirtualBase, CK_VirtualBase);
    GENERATE_CASE_PREFIX_ALT(r, CXXConstructionKind::, , Delegating, CK_Delegating);
#endif
  default:
    if (verbose_exn) { if (x) x->dump(); std::cout << "unknown construction kind: " << static_cast<int>(k) << DEBUG_SOURCE_RANGE(x) << std::endl;}
    caml_failwith("mlClangAST: unknown construction kind");
  }
  CAMLreturn(Val_int(r));
}


/* lambda_capture_kind */
enum {
  MLTAG_LCK_This,
  MLTAG_LCK_StarThis,
  MLTAG_LCK_ByCopy,
  MLTAG_LCK_ByRef,
  MLTAG_LCK_VLAType,
};

/* LambdaCapture -> lambda_capture */
CAMLprim value MLTreeBuilderVisitor::TranslateLambdaCapture(const LambdaCapture& x) {
  CAMLparam0();
  CAMLlocal1(ret);
  int r = 0;

  ret = caml_alloc_tuple(6);
  switch(x.getCaptureKind()) {
    GENERATE_CASE(r, LCK_This);
    GENERATE_CASE(r, LCK_StarThis);
    GENERATE_CASE(r, LCK_ByCopy);
    GENERATE_CASE(r, LCK_ByRef);
    GENERATE_CASE(r, LCK_VLAType);
  default:
    if (verbose_exn) { std::cout << "unknown lambda capture kind: " << x.getCaptureKind() << std::endl; }
    caml_failwith("mlClangAST: unknown lambda capture kind");
  }
  Store_field(ret, 0, Val_int(r));
  Store_field(ret, 1, Val_bool(x.capturesThis()));
  Store_field(ret, 2, Val_bool(x.capturesVLAType()));
  Store_field_option(ret, 3, x.capturesVariable(), TranslateDecl(x.getCapturedVar()));
  Store_field(ret, 4, Val_bool(x.isImplicit()));
  Store_field(ret, 5, Val_bool(x.isPackExpansion()));

  CAMLreturn(ret);
}




/* STATEMENTS */
/****************/


/* stmt */
enum {
  MLTAG_AttributedStmt,
  MLTAG_BreakStmt,
  MLTAG_CompoundStmt,
  MLTAG_ContinueStmt,
  MLTAG_DeclStmt,
  MLTAG_DoStmt,
  MLTAG_Expr, // ExprStmt in OCaml
  MLTAG_ForStmt,
  MLTAG_GotoStmt,
  MLTAG_IfStmt,
  MLTAG_IndirectGotoStmt,
  MLTAG_LabelStmt,
  MLTAG_ReturnStmt,
  MLTAG_CaseStmt,
  MLTAG_DefaultStmt,
  MLTAG_SwitchStmt,
  MLTAG_WhileStmt,

  /* C++ statememts */
  MLTAG_CXXForRangeStmt,
  MLTAG_CXXTryStmt,

  /* fallback */
  MLTAG_UnknownStmt,
};

/* stmt with no argument */
enum {
  MLTAG_AsmStmt,
  MLTAG_NullStmt,
};

/* Stmt -> stmt */
CAMLprim value MLTreeBuilderVisitor::TranslateStmt(const Stmt * node) {
  CAMLparam0();
  CAMLlocal2(ret,ret2);

  check_null(node, "TranslateStmt");

  if (verbose_alloc)
    std::cout << "TranslateStmt " << std::hex << node << std::dec << " " << node->getStmtClassName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  WITH_CACHE_TUPLE(cacheStmt, ret2, node, 2, {

      ret = Val_int(-1);

      GENERATE_NODE_CONSTANT(AsmStmt, ret, node);

      GENERATE_NODE(AttributedStmt, ret, node, 1, {
          Store_field(ret, 0, TranslateStmt(x->getSubStmt()));
        });

      GENERATE_NODE(BreakStmt, ret, node, 1, {
          Store_field(ret, 0, loc.TranslateSourceLocation(x->getBreakLoc()));
        });

      GENERATE_NODE(CompoundStmt, ret, node, 1, {
          Store_field_list(ret, 0, x->body(), TranslateStmt(child));
        });

      GENERATE_NODE(ContinueStmt, ret, node, 1, {
          Store_field(ret, 0, loc.TranslateSourceLocation(x->getContinueLoc()));
        });

      GENERATE_NODE(DeclStmt, ret, node, 1, {
          Store_field_list(ret, 0, x->decls(), TranslateDecl(child));
        });

      GENERATE_NODE_INDIRECT(DoStmt, ret, node, 2, {
          Store_field(ret, 0, TranslateStmt(x->getBody()));
          Store_field(ret, 1, TranslateExpr(x->getCond()));
        });

      GENERATE_NODE(Expr, ret, node, 1, {
          Store_field(ret, 0, TranslateExpr(x));
        });

      GENERATE_NODE_INDIRECT(ForStmt, ret, node, 4, {
          Store_field_option(ret, 0, x->getInit(), TranslateStmt(x->getInit()));
          Store_field_option(ret, 1, x->getCond(), TranslateExpr(x->getCond()));
          Store_field_option(ret, 2, x->getInc(), TranslateExpr(x->getInc()));
          Store_field(ret, 3, TranslateStmt(x->getBody()));
        });

      GENERATE_NODE(GotoStmt, ret, node, 2, {
          Store_field(ret, 0, TranslateNamedDecl(x->getLabel()->getStmt()->getDecl()));
          Store_field(ret, 1, loc.TranslateSourceLocation(x->getLabelLoc()));
        });

      GENERATE_NODE_INDIRECT(IfStmt, ret, node, 4, {
          Store_field_option(ret, 0, x->getCond(), TranslateExpr(x->getCond()));
          Store_field_option(ret, 1, x->getThen(), TranslateStmt(x->getThen()));
          Store_field_option(ret, 2, x->getElse(), TranslateStmt(x->getElse()));
          Store_field_option(ret, 3, x->getInit(), TranslateStmt(x->getInit()));
        });

      GENERATE_NODE(IndirectGotoStmt, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getTarget()));
          Store_field_option(ret, 1, x->getConstantTarget(), TranslateNamedDecl(x->getConstantTarget()->getStmt()->getDecl()));
        });

      GENERATE_NODE(LabelStmt, ret, node, 2, {
          Store_field(ret, 0, TranslateNamedDecl(x->getDecl()));
          Store_field(ret, 1, TranslateStmt(x->getSubStmt()));
        });

      GENERATE_NODE_CONSTANT(NullStmt, ret, node);

      GENERATE_NODE(ReturnStmt, ret, node, 1, {
          Store_field_option(ret, 0, x->getRetValue(), TranslateExpr(x->getRetValue()));
        });

      GENERATE_NODE_INDIRECT(CaseStmt, ret, node, 3, {
          CAMLenterblock();
          value tmp = Val_unit;
          value tmp2 = Val_unit;
          value tmp3 = Val_unit;
          value tmp4 = Val_unit;
          CAMLxparam4(tmp,tmp2,tmp3,tmp4);
          const Stmt* s;

          Store_field(ret, 0, TranslateExpr(x->getLHS()));
          Store_field_option(ret, 1, x->getRHS(), TranslateExpr(x->getRHS()));
          tmp = ret;
          // optimize long sequences of cases using a loop instead of
          // recursive calls
          s = x->getSubStmt();
          while (isa<CaseStmt>(s)) {
            x = cast<CaseStmt>(s);

            tmp2 = caml_alloc_tuple(3);
            Store_field(tmp2, 0, TranslateExpr(x->getLHS()));
            Store_field_option(tmp2, 1, x->getRHS(), TranslateExpr(x->getRHS()));
            // field 2 to be filled in the next iteration, or after the loop

            tmp3 = caml_alloc(1, MLTAG_CaseStmt);
            Store_field(tmp3, 0, tmp2);

            tmp4 = caml_alloc_tuple(2);
            Store_field(tmp4, 0, tmp3);
            Store_field(tmp4, 1, loc.TranslateSourceRange(x->getSourceRange()));

            Store_field(tmp, 2, tmp4);
            tmp = tmp2;
            s = x->getSubStmt();
          }
          Store_field(tmp, 2, TranslateStmt(s));

          CAMLexitblock();
        });

      GENERATE_NODE(DefaultStmt, ret, node, 1, {
          Store_field(ret, 0, TranslateStmt(x->getSubStmt()));
        });

      GENERATE_NODE_INDIRECT(SwitchStmt, ret, node, 3, {
          Store_field_option(ret, 0, x->getInit(), TranslateStmt(x->getInit()));
          Store_field(ret, 1, TranslateExpr(x->getCond()));
          Store_field(ret, 2, TranslateStmt(x->getBody()));
        });


      GENERATE_NODE_INDIRECT(WhileStmt, ret, node, 2, {
          Store_field(ret, 0, TranslateExpr(x->getCond()));
          Store_field(ret, 1, TranslateStmt(x->getBody()));
        });

       GENERATE_NODE_INDIRECT(CXXForRangeStmt, ret, node, 3, {
           Store_field(ret, 0, TranslateVarDecl(x->getLoopVariable()));
           Store_field(ret, 1, TranslateExpr(x->getRangeInit()));
           Store_field(ret, 2, TranslateStmt(x->getBody()));
       });

       GENERATE_NODE_INDIRECT(CXXTryStmt, ret, node, 2, {
           Store_field(ret, 0, TranslateStmt(x->getTryBlock()));
           Store_field_array(ret, 1, x->getNumHandlers(), TranslateCXXCatchStmt(x->getHandler(i)));
        });


      // Default
      if (ret == Val_int(-1)) {
        ret = caml_alloc(2, MLTAG_UnknownStmt);
        Store_field(ret, 0, Val_int(node->getStmtClass()));
        Store_field(ret, 1, caml_copy_string(node->getStmtClassName()));
        if (log_unknown) { std::cerr << "mlClangAST: unhandled Stmt node encountered: " << node->getStmtClassName() << std::endl; }
      }

      Store_field(ret2, 0, ret);
      Store_field(ret2, 1, loc.TranslateSourceRange(node->getSourceRange()));
    });

  if (verbose_alloc)
    std::cout << "end TranslateStmt " << std::hex << node << std::dec << " " << node->getStmtClassName() << DEBUG_SOURCE_RANGE(node) << std::endl;

  CAMLreturn(ret2);
}


/* Stmt -> stmt */
CAMLprim value MLTreeBuilderVisitor::TranslateCXXCatchStmt(const CXXCatchStmt * x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TranslateCXXCacheStmt");
  ret = caml_alloc_tuple(3);
  Store_field_option(ret, 0, x->getExceptionDecl(), TranslateVarDecl(x->getExceptionDecl()));
  Store_field_option(ret, 1,  x->getExceptionDecl(), TranslateQualType(x->getCaughtType()));
  Store_field(ret, 2, TranslateStmt(x->getHandlerBlock()));

  CAMLreturn(ret);
}

bool MLTreeBuilderVisitor::TraverseStmt(Stmt *node) {
  return false;
}



/* TYPES */
/*********/


/* builtin_type */
enum {
  MLTAG_Type_Void,
  MLTAG_Type_Bool,
  MLTAG_Type_Char_U,
  MLTAG_Type_UChar,
  MLTAG_Type_WChar_U,
  MLTAG_Type_Char16,
  MLTAG_Type_Char32,
  MLTAG_Type_UShort,
  MLTAG_Type_UInt,
  MLTAG_Type_ULong,
  MLTAG_Type_ULongLong,
  MLTAG_Type_UInt128,
  MLTAG_Type_Char_S,
  MLTAG_Type_SChar,
  MLTAG_Type_WChar_S,
  MLTAG_Type_Short,
  MLTAG_Type_Int,
  MLTAG_Type_Long,
  MLTAG_Type_LongLong,
  MLTAG_Type_Int128,
  MLTAG_Type_Half,
  MLTAG_Type_Float,
  MLTAG_Type_Double,
  MLTAG_Type_LongDouble,
  MLTAG_Type_Float128,
  MLTAG_Type_NullPtr,
  MLTAG_Type_ObjCId,
  MLTAG_Type_ObjCClass,
  MLTAG_Type_ObjCSel,
  MLTAG_Type_OCLSampler,
  MLTAG_Type_OCLEvent,
  MLTAG_Type_OCLClkEvent,
  MLTAG_Type_OCLQueue,
  MLTAG_Type_OCLReserveID,
  MLTAG_Type_Dependent,
  MLTAG_Type_Overload,
  MLTAG_Type_BoundMember,
  MLTAG_Type_PseudoObject,
  MLTAG_Type_UnknownAny,
  MLTAG_Type_BuiltinFn,
  MLTAG_Type_ARCUnbridgedCast,
  MLTAG_Type_OMPArraySection,
};


/* BuiltinType -> builtin_type */
CAMLprim value MLTreeBuilderVisitor::TranslateBuiltinType(const BuiltinType * node) {
  CAMLparam0();
  int r = 0;

  check_null(node, "TranslateBuiltinType");

  switch(node->getKind()) {
    // void
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Void);
    // bool
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Bool);
    // unsigned
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Char_U);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, UChar);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, WChar_U);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Char16);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Char32);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, UShort);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, UInt);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ULong);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ULongLong);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, UInt128);
    // signed
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Char_S);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, SChar);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, WChar_S);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Short);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Int);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Long);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, LongLong);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Int128);
    // float
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Half);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Float);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Double);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, LongDouble);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Float128);
    // others
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, NullPtr);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ObjCId);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ObjCClass);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ObjCSel);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OCLSampler);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OCLEvent);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OCLClkEvent);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OCLQueue);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OCLReserveID);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Dependent);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, Overload);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, BoundMember);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, PseudoObject);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, UnknownAny);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, BuiltinFn);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, ARCUnbridgedCast);
    GENERATE_CASE_PREFIX(r, BuiltinType::, Type_, OMPArraySection);
    // unknown
  default:
    if (verbose_exn) { node->dump(); std::cout << "unknown builtin type: " << node->getKind() << std::endl; }
    caml_failwith("mlClangAST: unknown builtin type");
  }
  CAMLreturn(Val_int(r));
}

/* exception_specification_type */
enum {
  MLTAG_EST_None,
  MLTAG_EST_DynamicNone,
  MLTAG_EST_Dynamic,
  MLTAG_EST_MSAny,
  MLTAG_EST_BasicNoexcept,
  MLTAG_EST_ComputedNoexcept,
  MLTAG_EST_Unevaluated,
  MLTAG_EST_Uninstantiated,
  MLTAG_EST_Unparsed,
  // version >= 7
  MLTAG_EST_DependentNoexcept,
  MLTAG_EST_NoexceptFalse,
  MLTAG_EST_NoexceptTrue,
};

/* noexcept_result */
enum {
  MLTAG_NR_NoNoexcept,
  MLTAG_NR_BadNoexcept,
  MLTAG_NR_Dependent,
  MLTAG_NR_Throw,
  MLTAG_NR_Nothrow,
};

/* FunctionProtoType -> fun_proto_type */
CAMLprim value MLTreeBuilderVisitor::TranslateFunctionProtoType(const FunctionProtoType * node) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);
  int r = 0;

  check_null(node, "TranslateFunctionProtoType");

  WITH_CACHE_TUPLE(cacheMisc, ret, node, 10, {

      Store_field(ret, 0, TranslateQualType(node->getCallResultType(*Context)));
      Store_field(ret, 1, TranslateQualType(node->getReturnType()));

      tmp = caml_alloc_tuple(3);
      Store_field(tmp, 0, Val_bool(node->isConst()));
      Store_field(tmp, 1, Val_bool(node->isRestrict()));
      Store_field(tmp, 2, Val_bool(node->isVolatile()));
      Store_field(ret, 2, tmp);

      Store_field_array(ret, 3, node->getNumParams(), TranslateQualType(node->getParamType(i)));
      Store_field(ret, 4, Val_bool(node->isVariadic()));

      // C++
      switch (node->getExceptionSpecType()) {
        GENERATE_CASE(r, EST_None);
        GENERATE_CASE(r, EST_DynamicNone);
        GENERATE_CASE(r, EST_Dynamic);
        GENERATE_CASE(r, EST_MSAny);
        GENERATE_CASE(r, EST_BasicNoexcept);
#if CLANG_VERSION_MAJOR >= 7
        GENERATE_CASE(r, EST_DependentNoexcept);
        GENERATE_CASE(r, EST_NoexceptFalse);
        GENERATE_CASE(r, EST_NoexceptTrue);
#else
        GENERATE_CASE(r, EST_ComputedNoexcept);
#endif
        GENERATE_CASE(r, EST_Unevaluated);
        GENERATE_CASE(r, EST_Uninstantiated);
        GENERATE_CASE(r, EST_Unparsed);
      default:
        if (verbose_exn) { node->dump(); std::cout << "unknown exception spec type: " << node->getExceptionSpecType() << std::endl; }
        caml_failwith("mlClangAST: unknown exception spec type");
      }
      Store_field(ret, 5, Val_int(r));

#if CLANG_VERSION_MAJOR >= 7
      // no getNoexceptSpec method
      r = MLTAG_NR_NoNoexcept;
#else
      switch (node->getNoexceptSpec(*Context)) {
        GENERATE_CASE_PREFIX(r, FunctionProtoType::, , NR_NoNoexcept);
        GENERATE_CASE_PREFIX(r, FunctionProtoType::, , NR_BadNoexcept);
        GENERATE_CASE_PREFIX(r, FunctionProtoType::, , NR_Dependent);
        GENERATE_CASE_PREFIX(r, FunctionProtoType::, , NR_Throw);
        GENERATE_CASE_PREFIX(r, FunctionProtoType::, , NR_Nothrow);
      default:
        if (verbose_exn) { node->dump(); std::cout << "unknown noexcept spec: " << node->getNoexceptSpec(*Context) << DEBUG_SOURCE_RANGE(node) << std::endl; }
        caml_failwith("mlClangAST: unknown noexcept spec");
      }
#endif
      Store_field(ret, 6, Val_int(r));

      Store_field_array(ret, 7, node->getNumExceptions(), TranslateQualType(node->getExceptionType(i)));
      Store_field(ret, 8, Val_bool(node->hasTrailingReturn()));
      Store_field(ret, 9, TranslateRefQualifierKind(node->getRefQualifier()));
    });

  CAMLreturn(ret);
}


/* FunctionNoProtoType -> fun_no_proto_type */
CAMLprim value MLTreeBuilderVisitor::TranslateFunctionNoProtoType(const FunctionNoProtoType * node) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(node, "TranslateFunctionNoProtoType");

  /* valgrind shows that the qualifier information is not (always?) set
     for prototypes -> we do not export this information
   */
  WITH_CACHE_TUPLE(cacheMisc, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(node->getCallResultType(*Context)));
    });

  CAMLreturn(ret);
}


/* array_size_modifier */
enum {
  MLTAG_SIZE_NORMAL,
  MLTAG_SIZE_STATIC,
  MLTAG_SIZE_STAR,
};

/* array_size, with argument */
enum {
  MLTAG_ConstantArrayType,
  MLTAG_VariableArrayType,
};

/* array_size, without argument */
enum {
  MLTAG_IncompleteArrayType,
  MLTAG_DependentSizedArrayType,
};

/* ArrayType -> array_type */
CAMLprim value MLTreeBuilderVisitor::TranslateArrayType(const ArrayType * node) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(node, "TranslateArrayType");

  WITH_CACHE_TUPLE(cacheMisc, ret, node, 3, {
      int r;
      Store_field(ret, 0, TranslateQualType(node->getElementType()));

      tmp = Val_int(-1);
      GENERATE_NODE(ConstantArrayType, tmp, node, 1, {
          Store_field(tmp, 0, TranslateAPInt(x->getSize()));
        });
      GENERATE_NODE(VariableArrayType, tmp, node, 1, {
          Store_field_option(tmp, 0, x->getSizeExpr(), TranslateExpr(x->getSizeExpr()));
        });
      GENERATE_NODE_CONSTANT(IncompleteArrayType, tmp, node);
      GENERATE_NODE_CONSTANT(DependentSizedArrayType, tmp, node);
      if (tmp == Val_int(-1)) {
        if (verbose_exn) node->dump();
        caml_failwith("mlClangAST: unknown array type");
      }
      Store_field(ret, 1, tmp);

      switch (node->getSizeModifier()) {
#if CLANG_VERSION_MAJOR < 18
      case ArrayType::Normal: r = MLTAG_SIZE_NORMAL; break;
      case ArrayType::Static: r = MLTAG_SIZE_STATIC; break;
      case ArrayType::Star: r = MLTAG_SIZE_STAR; break;
#else
      case ArraySizeModifier::Normal: r = MLTAG_SIZE_NORMAL; break;
      case ArraySizeModifier::Static: r = MLTAG_SIZE_STATIC; break;
      case ArraySizeModifier::Star: r = MLTAG_SIZE_STAR; break;
#endif
      default:
        if (verbose_exn) { node->dump(); std::cout << "unknown array size modifier: " << static_cast<int>(node->getSizeModifier()) << std::endl; }
        caml_failwith("mlClangAST: unknown array size modifier");
      }
      Store_field(ret, 2, Val_int(r));
    });

  CAMLreturn(ret);
}


/* typ */
enum {
  // C
  MLTAG_DecayedType,
  MLTAG_ArrayType,
  MLTAG_AtomicType,
  MLTAG_AttributedType,
  MLTAG_BuiltinType,
  MLTAG_ComplexType,
  MLTAG_FunctionProtoType,
  MLTAG_FunctionNoProtoType,
  MLTAG_ParenType,
  MLTAG_PointerType,
  MLTAG_EnumType,
  MLTAG_RecordType,
  MLTAG_TypedefType,
  MLTAG_ElaboratedType,
  MLTAG_UnaryTransformType,
  MLTAG_TypeOfExprType,
  MLTAG_TypeOfType,

  // C++
  MLTAG_DecltypeType,
  MLTAG_AutoType,
  MLTAG_DeducedTemplateSpecializationType,
  MLTAG_DependentSizedExtVectorType,
  MLTAG_InjectedClassNameType,
  MLTAG_MemberPointerType,
  MLTAG_PackExpansionType,
  MLTAG_LValueReferenceType,
  MLTAG_RValueReferenceType,
  MLTAG_SubstTemplateTypeParmPackType,
  MLTAG_SubstTemplateTypeParmType,
  MLTAG_TemplateSpecializationType,
  MLTAG_TemplateTypeParmType,
  MLTAG_DependentNameType,
  MLTAG_DependentTemplateSpecializationType,
  MLTAG_UnresolvedUsingType,

  // Vectors
  MLTAG_VectorType,

  // Unknown
  MLTAG_UnknownType,
};

/* Type -> type */
CAMLprim value MLTreeBuilderVisitor::TranslateType(const Type * node) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(node, "TranslateType");
  if (verbose_alloc)
    std::cout << "TranslateType " << std::hex << node << std::dec << " " << node->getTypeClassName() << std::endl;

  ret = Val_int(-1);

  GENERATE_NODE_CACHED(cacheType, DecayedType, ret, node, 2, {
      Store_field(ret, 0, TranslateQualType(x->getDecayedType()));
      Store_field(ret, 1, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, ArrayType, ret, node, 1, {
      Store_field(ret, 0, TranslateArrayType(x));
    });

  GENERATE_NODE_CACHED(cacheType, AtomicType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getValueType()));
    });

  GENERATE_NODE_CACHED(cacheType, AttributedType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getModifiedType()));
      // TODO: put attributes
    });

  GENERATE_NODE_CACHED(cacheType, BuiltinType, ret, node, 1, {
      Store_field(ret, 0, TranslateBuiltinType(x));
    });

  GENERATE_NODE_CACHED(cacheType, ComplexType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getElementType()));
    });

  GENERATE_NODE_CACHED(cacheType, FunctionProtoType, ret, node, 1, {
      Store_field(ret, 0, TranslateFunctionProtoType(x));
    });

  GENERATE_NODE_CACHED(cacheType, FunctionNoProtoType, ret, node, 1, {
      Store_field(ret, 0, TranslateFunctionNoProtoType(x));
    });

  GENERATE_NODE_CACHED(cacheType, ParenType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getInnerType()));
    });

  GENERATE_NODE_CACHED(cacheType, PointerType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, EnumType, ret, node, 1, {
      Store_field(ret, 0, TranslateEnumDecl(x->getDecl()));
    });

  GENERATE_NODE_CACHED(cacheType, RecordType, ret, node, 1, {
      Store_field(ret, 0, TranslateRecordDecl(x->getDecl()));
    });

  GENERATE_NODE_CACHED(cacheType, TypedefType, ret, node, 1, {
      Store_field(ret, 0, TranslateTypedefNameDecl(x->getDecl()));
    });

  GENERATE_NODE_CACHED(cacheType, ElaboratedType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getNamedType()));
    });

  GENERATE_NODE_CACHED(cacheType, UnaryTransformType, ret, node, 3, {
      Store_field(ret, 0, TranslateQualType(x->getUnderlyingType()));
      Store_field(ret, 1, TranslateQualType(x->getBaseType()));
      Store_field(ret, 2, Val_false);
   });

  GENERATE_NODE_CACHED(cacheType,TypeOfExprType, ret, node, 1, {
      Store_field(ret, 0, TranslateExpr(x->getUnderlyingExpr()));
    });

  GENERATE_NODE_CACHED(cacheType,TypeOfType, ret, node, 1, {
#if CLANG_VERSION_MAJOR >= 16
      Store_field(ret, 0, TranslateQualType(x->desugar()));
#else
      Store_field(ret, 0, TranslateQualType(x->getUnderlyingType()));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, DecltypeType, ret, node, 2, {
      Store_field(ret, 0, TranslateExpr(x->getUnderlyingExpr()));
      Store_field(ret, 1, TranslateQualTypeOption(x->getUnderlyingType()));
    });

  GENERATE_NODE_CACHED(cacheType, AutoType, ret, node, 1, {
      Store_field(ret, 0, Val_bool(x->isDecltypeAuto()));
    });

#if CLANG_VERSION_MAJOR >= 5
  GENERATE_NODE_CACHED(cacheType, DeducedTemplateSpecializationType, ret, node, 1, {
      Store_field(ret, 0, TranslateTemplateName(x->getTemplateName()));
    });
#endif

  GENERATE_NODE_CACHED(cacheType, DependentSizedExtVectorType, ret, node, 2, {
      Store_field(ret, 0, TranslateExpr(x->getSizeExpr()));
      Store_field(ret, 1, TranslateQualType(x->getElementType()));
    });

  GENERATE_NODE_CACHED(cacheType, InjectedClassNameType, ret, node, 4, {
      const TemplateSpecializationType* t = x->getInjectedTST();
      Store_field(ret, 0, TranslateQualType(x->getInjectedSpecializationType()));
#if CLANG_VERSION_MAJOR >= 5
      Store_field_option(ret, 1, true, TranslateTemplateName(x->getTemplateName()));
#else
      Store_field(ret, 1, Val_unit);
#endif
      Store_field(ret, 2, TranslateRecordDecl(x->getDecl()));
#if CLANG_VERSION_MAJOR >= 16
      ArrayRef<TemplateArgument> d = t->template_arguments();
      Store_field_array(ret, 3, d.size(), TranslateTemplateArgument(d[i]));
#else
      Store_field_array(ret, 3, t->getNumArgs(), TranslateTemplateArgument(t->getArg(i)));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, MemberPointerType, ret, node, 2, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
      Store_field(ret, 1, TranslateType(x->getClass()));
    });

  GENERATE_NODE_CACHED(cacheType, PackExpansionType, ret, node, 2, {
      Store_field(ret, 0, TranslateQualType(x->getPattern()));
      const Optional<unsigned> num = x->getNumExpansions();
#if CLANG_VERSION_MAJOR >= 16
      Store_field_option(ret, 1, num.has_value(), Val_int(num.value()));
#else
      Store_field_option(ret, 1, num.hasValue(), Val_int(num.getValue()));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, LValueReferenceType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, RValueReferenceType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, SubstTemplateTypeParmPackType, ret, node, 3, {
      Store_field(ret, 0, caml_copy_string(x->getIdentifier()->getNameStart()));
#if CLANG_VERSION_MAJOR >= 16
      Store_field(ret, 1, TranslateTemplateTypeParmDecl(x->getReplacedParameter()));
#else
      Store_field(ret, 1, TranslateTemplateTypeParmDecl(x->getReplacedParameter()->getDecl()));
#endif
      Store_field(ret, 2, TranslateTemplateArgument(x->getArgumentPack()));
    });

  GENERATE_NODE_CACHED(cacheType, SubstTemplateTypeParmType, ret, node, 2, {
#if CLANG_VERSION_MAJOR >= 16
      Store_field(ret, 0, TranslateTemplateTypeParmDecl(x->getReplacedParameter()));
#else
      Store_field(ret, 0, TranslateTemplateTypeParmDecl(x->getReplacedParameter()->getDecl()));
#endif
      Store_field(ret, 1, TranslateQualType(x->getReplacementType()));
    });

  GENERATE_NODE_CACHED(cacheType, TemplateSpecializationType, ret, node, 3, {
      Store_field_option(ret, 0, x->isTypeAlias(), TranslateQualType(x->getAliasedType()));
      Store_field(ret, 1, TranslateTemplateName(x->getTemplateName()));

    });

  GENERATE_NODE_CACHED(cacheType, MemberPointerType, ret, node, 2, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
      Store_field(ret, 1, TranslateType(x->getClass()));
    });

  GENERATE_NODE_CACHED(cacheType, PackExpansionType, ret, node, 2, {
      Store_field(ret, 0, TranslateQualType(x->getPattern()));
      const Optional<unsigned> num = x->getNumExpansions();
#if CLANG_VERSION_MAJOR >= 16
      Store_field_option(ret, 1, num.has_value(), Val_int(num.value()));
#else
      Store_field_option(ret, 1, num.hasValue(), Val_int(num.getValue()));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, LValueReferenceType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, RValueReferenceType, ret, node, 1, {
      Store_field(ret, 0, TranslateQualType(x->getPointeeType()));
    });

  GENERATE_NODE_CACHED(cacheType, SubstTemplateTypeParmPackType, ret, node, 3, {
      Store_field(ret, 0, caml_copy_string(x->getIdentifier()->getNameStart()));
#if CLANG_VERSION_MAJOR >= 16
      Store_field(ret, 1, TranslateTemplateTypeParmDecl(x->getReplacedParameter()));
#else
      Store_field(ret, 1, TranslateTemplateTypeParmDecl(x->getReplacedParameter()->getDecl()));
#endif
      Store_field(ret, 2, TranslateTemplateArgument(x->getArgumentPack()));
    });

  GENERATE_NODE_CACHED(cacheType, SubstTemplateTypeParmType, ret, node, 2, {
#if CLANG_VERSION_MAJOR >= 16
      Store_field(ret, 0, TranslateTemplateTypeParmDecl(x->getReplacedParameter()));
#else
      Store_field(ret, 0, TranslateTemplateTypeParmDecl(x->getReplacedParameter()->getDecl()));
#endif
      Store_field(ret, 1, TranslateQualType(x->getReplacementType()));
    });

  GENERATE_NODE_CACHED(cacheType, TemplateSpecializationType, ret, node, 3, {
      Store_field_option(ret, 0, x->isTypeAlias(), TranslateQualType(x->getAliasedType()));
      Store_field(ret, 1, TranslateTemplateName(x->getTemplateName()));
#if CLANG_VERSION_MAJOR >= 16
      ArrayRef<TemplateArgument> d = x->template_arguments();
      Store_field_array(ret, 2, d.size(), TranslateTemplateArgument(d[i]));
#else
      Store_field_array(ret, 2, x->getNumArgs(), TranslateTemplateArgument(x->getArg(i)));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, TemplateTypeParmType, ret, node, 1, {
      Store_field(ret, 0, TranslateTemplateTypeParmType(x));
    });

  GENERATE_NODE_CACHED(cacheType, DependentNameType, ret, node, 2, {
      Store_field(ret, 0, TranslateNestedNameSpecifierList(x->getQualifier()));
      Store_field(ret, 1, caml_copy_string(x->getIdentifier()->getNameStart()));
    });

  GENERATE_NODE_CACHED(cacheType, DependentTemplateSpecializationType, ret, node, 3, {
      Store_field(ret, 0, TranslateNestedNameSpecifierList(x->getQualifier()));
      Store_field(ret, 1, caml_copy_string(x->getIdentifier()->getNameStart()));
#if CLANG_VERSION_MAJOR >= 16
      ArrayRef<TemplateArgument> d = x->template_arguments();
      Store_field_array(ret, 2, d.size(), TranslateTemplateArgument(d[i]));
#else
      Store_field_array(ret, 2, x->getNumArgs(), TranslateTemplateArgument(x->getArg(i)));
#endif
    });

  GENERATE_NODE_CACHED(cacheType, UnresolvedUsingType, ret, node, 1, {
      Store_field(ret, 0, TranslateUnresolvedUsingTypenameDecl(x->getDecl()));
    });

  GENERATE_NODE_CACHED(cacheType, VectorType, ret, node, 3, {
      Store_field(ret, 0, TranslateQualType(x->getElementType()));
      Store_field(ret, 1, Val_int(x->getNumElements()));
      Store_field(ret, 2, Val_int(x->getVectorKind()));
    });


  // Default
  if (ret == Val_int(-1)) {
    WITH_CACHE(cacheType, ret, node, 2, MLTAG_UnknownType, {
        Store_field(ret, 0, Val_int(node->getTypeClass()));
        Store_field(ret, 1, caml_copy_string(node->getTypeClassName()));
        if (log_unknown) { std::cerr << "mlClangAST: unhandled Type node encountered: " << node->getTypeClassName() << std::endl; }
    });
  }

  if (verbose_alloc)
    std::cout << "end TranslateType " << std::hex << node << std::dec << " " << node->getTypeClassName() << std::endl;

  CAMLreturn(ret);
}

/* TemplateTypeParmType -> template_type_param_type */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateTypeParmType(const TemplateTypeParmType* x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TemplateTypeParmType");
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 5, {
      Store_field(ret, 0, Val_int(x->getDepth()));
      Store_field(ret, 1, Val_int(x->getIndex()));
      Store_field(ret, 2, Val_bool(x->isParameterPack()));
      Store_field_option(ret, 3, x->getDecl(), TranslateTemplateTypeParmDecl(x->getDecl()));
      Store_field_option(ret, 4, x->getIdentifier() && x->getIdentifier()->getNameStart(),
                         caml_copy_string(x->getIdentifier()->getNameStart()));
    });
  CAMLreturn(ret);
}

/* TemplateTypeParmDecl -> template_type_param_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateTemplateTypeParmDecl(const TemplateTypeParmDecl* x) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(x, "TemplateTypeParmDecl");
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 2, {
      Store_field(ret, 0, TranslateNamedDecl(x));
      Store_field_option(ret, 1, x->hasDefaultArgument(), TranslateQualType(x->getDefaultArgument()));
    });
  CAMLreturn(ret);
}

/* Type -> type option */
CAMLprim value MLTreeBuilderVisitor::TranslateTypeOption(const Type * x) {
  CAMLparam0();
  CAMLlocal1(ret);
  GENERATE_OPTION(ret, x, TranslateType(x));
  CAMLreturn(ret);
}

/* QualType -> type_qual */
CAMLprim value MLTreeBuilderVisitor::TranslateQualType(QualType x) {
  CAMLparam0();
  CAMLlocal2(ret,qual);

  void* o = x.getAsOpaquePtr();
  check_null(o, "TranslateQualType");

  CACHED(cacheTypeQual, ret, o, {
      qual = caml_alloc_tuple(3);
      Store_field(qual, 0, Val_bool(x.isConstQualified()));
      Store_field(qual, 1, Val_bool(x.isRestrictQualified()));
      Store_field(qual, 2, Val_bool(x.isVolatileQualified()));
      ret = caml_alloc_tuple(2);
      Store_field(ret, 0, TranslateType(x.getTypePtr()));
      Store_field(ret, 1, qual);
    });

  CAMLreturn(ret);
}

/* ref_qualifier_kind */
enum {
  MLTAG_RQ_None,
  MLTAG_RQ_LValue,
  MLTAG_RQ_RValue,
};

/* RefQualifierKind -> ref_qualifier */
CAMLprim value MLTreeBuilderVisitor::TranslateRefQualifierKind(RefQualifierKind x) {
  value ret;
  switch (x) {
    GENERATE_CASE(ret, RQ_None);
    GENERATE_CASE(ret, RQ_LValue);
    GENERATE_CASE(ret, RQ_RValue);
  default:
    if (verbose_exn) std::cout << "unknown ref qualifier kind: " << x << std::endl;
    caml_failwith("mlClangAST: unknown ref qualifier kind");
  }
  return Val_int(ret);
}

/* QualType -> qual_type option */
CAMLprim value MLTreeBuilderVisitor::TranslateQualTypeOption(QualType x) {
  CAMLparam0();
  CAMLlocal1(ret);
  GENERATE_OPTION(ret, !x.isNull(), TranslateQualType(x));
  CAMLreturn(ret);
}


/* EnumConstantDecl -> enum_cst */
CAMLprim value MLTreeBuilderVisitor::TranslateEnumConstantDecl(const EnumConstantDecl* org) {
  CAMLparam0();
  CAMLlocal1(ret);

  check_null(org, "TranslateEnumConstantDecl");
  const EnumConstantDecl* x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 5, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field(ret, 2, TranslateAPSInt(x->getInitVal()));
      Store_field(ret, 3, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 4, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
    });
  CAMLreturn(ret);
}

/* EnumDecl -> enum_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateEnumDecl(const EnumDecl * org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "TranslateEnumDecl");
  const EnumDecl * x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 11, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field(ret, 2, Val_int(x->getNumPositiveBits()));
      Store_field(ret, 3, Val_int(x->getNumNegativeBits()));
      Store_field(ret, 4, Val_bool(x->isComplete()));
      Store_field(ret, 5, TranslateQualTypeOption(x->getIntegerType()));
      Store_field(ret, 6, TranslateQualTypeOption(x->getPromotionType()));
      Store_field_list(ret, 7, x->enumerators(), TranslateEnumConstantDecl(child));
      Store_field_option(ret, 8, x->getTypedefNameForAnonDecl(), TranslateTypedefNameDecl(x->getTypedefNameForAnonDecl()));
      Store_field(ret, 9, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 10, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
    });
  CAMLreturn(ret);
}

/* FieldDecl -> field_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateFieldDecl(const FieldDecl * org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "TranslateFieldDecl");
  const FieldDecl * x = org;
  //x = x->getCanonicalDecl();
  WITH_CACHE_TUPLE(cacheMisc2, ret, x, 12, {
      const RecordDecl* d = x->getParent();
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field(ret, 2, Val_int(x->getFieldIndex()));
      Store_field(ret, 3, TranslateQualType(x->getType()));
      Store_field_option(ret, 4, x->isBitField(), Val_int(x->getBitWidthValue(*Context)));
      Store_field(ret, 5, Val_bool(x->isUnnamedBitfield()));
      if (d->isCompleteDefinition() && !d->isInvalidDecl() && !d->isDependentType()) {
        // valid layout
        const ASTRecordLayout & l = Context->getASTRecordLayout(d);
        Store_field(ret, 6, Val_true);
        Store_field(ret, 7, caml_copy_int64(l.getFieldOffset(x->getFieldIndex())));
      }
      else {
        // no layout information: set to 0
        Store_field(ret, 6, Val_false);
        Store_field(ret, 7, caml_copy_int64(0));
      }
      Store_field_option(ret, 8, x->hasCapturedVLAType(), TranslateExpr(x->getCapturedVLAType()->getSizeExpr()));
      Store_field(ret, 9, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 10, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
    });
  CAMLreturn(ret);
}

/* record_kind */
enum {
  MLTAG_TTK_Struct,
  MLTAG_TTK_Union,
  MLTAG_TTK_Class,
  MLTAG_TTK_Interface,
};

/* RecordDecl -> record_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateRecordDecl(const RecordDecl * org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "TranslateRecordDecl");
  const RecordDecl *x = org;
  //x = dyn_cast<RecordDecl>(x->getCanonicalDecl());
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 19, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      int kind;
      switch (x->getTagKind()) {
#if CLANG_VERSION_MAJOR < 18
        GENERATE_CASE(kind, TTK_Struct);
        GENERATE_CASE(kind, TTK_Union);
        GENERATE_CASE(kind, TTK_Class);
        GENERATE_CASE(kind, TTK_Interface);
#else
        GENERATE_CASE_PREFIX(kind, TagTypeKind::, TTK_, Struct);
        GENERATE_CASE_PREFIX(kind, TagTypeKind::, TTK_, Union);
        GENERATE_CASE_PREFIX(kind, TagTypeKind::, TTK_, Class);
        GENERATE_CASE_PREFIX(kind, TagTypeKind::, TTK_, Interface);
#endif
      default:
        if (verbose_exn) { x->dump(); std::cout << "unknown record kind: " << static_cast<int>(x->getTagKind()) << DEBUG_SOURCE_RANGE(org) << std::endl; }
        caml_failwith("mlClangAST: unknown record kind");
      }
      Store_field(ret, 2, Val_int(kind));
      Store_field(ret, 3, Val_bool(x->hasFlexibleArrayMember()));
      Store_field(ret, 4, Val_bool(x->hasVolatileMember()));
      Store_field(ret, 5, Val_bool(x->isAnonymousStructOrUnion()));
      Store_field(ret, 6, Val_bool(x->isCompleteDefinition()));
      Store_field(ret, 7, Val_bool(!x->isInvalidDecl()));
      if (x->isCompleteDefinition() && !x->isInvalidDecl() && !x->isDependentType()) {
        // valid layout
        const ASTRecordLayout & l = Context->getASTRecordLayout(x);
        Store_field(ret, 8, caml_copy_int64(l.getSize().getQuantity()));
        Store_field(ret, 9, caml_copy_int64(l.getDataSize().getQuantity()));
        Store_field(ret, 10, caml_copy_int64(l.getAlignment().getQuantity()));
      }
      else {
        // no layout information: set to 0
        Store_field(ret, 8, caml_copy_int64(0));
        Store_field(ret, 9, caml_copy_int64(0));
        Store_field(ret, 10, caml_copy_int64(0));
      }
      Store_field_list(ret, 11, x->fields(), TranslateFieldDecl(child));
      Store_field_option(ret, 12, x->getTypedefNameForAnonDecl(), TranslateTypedefNameDecl(x->getTypedefNameForAnonDecl()));
      Store_field(ret, 13, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 14, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));

      // template information
      Store_field_option(ret, 15, isa<ClassTemplateSpecializationDecl>(x),
                         TranslateClassTemplateSpecializationDecl(cast<ClassTemplateSpecializationDecl>(x)));
      // class information
      Store_field(ret, 16, Val_false);
      Store_field(ret, 17, Val_false);
      Store_field(ret, 18, Val_false);
      if (x->isCompleteDefinition() && isa<CXXRecordDecl>(x)) {
        const CXXRecordDecl* d = cast<CXXRecordDecl>(x);
        Store_field_list(ret, 16, d->bases(), TranslateCXXBaseSpecifier(child));
        Store_field_list(ret, 17, d->methods(), TranslateFunctionDecl(child));
        Store_field_list(ret, 18, d->friends(), TranslateFriendDecl(child));
      }
    });
  CAMLreturn(ret);
}

/* CXXBaseSpecifier -> cxx_base_specifier */
CAMLprim value MLTreeBuilderVisitor::TranslateCXXBaseSpecifier(const CXXBaseSpecifier& x) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(6);
  Store_field(ret, 0, Val_bool(x.isVirtual()));
  Store_field(ret, 1, Val_bool(x.isBaseOfClass()));
  Store_field(ret, 2, Val_bool(x.isPackExpansion()));
  Store_field(ret, 3, Val_bool(x.getInheritConstructors()));
  Store_field(ret, 4, TranslateAccessSpecifier(x.getAccessSpecifier()));
  Store_field(ret, 5, TranslateQualType(x.getType()));

  CAMLreturn(ret);
}


bool MLTreeBuilderVisitor::TraverseType(QualType node) {
  return false;
}

/* TypedefDecl -> typedef_decl */
CAMLprim value MLTreeBuilderVisitor::TranslateTypedefNameDecl(const TypedefNameDecl * org) {
  CAMLparam0();
  CAMLlocal2(ret,tmp);

  check_null(org, "TranslateTypedefNameDecl");
  const TypedefNameDecl * x = org;
  // NOTE: the canonical decl may sometimes miss its name!
  // x = dyn_cast<TypedefDecl>(x->getCanonicalDecl());
  WITH_CACHE_TUPLE(cacheMisc, ret, x, 5, {
      Store_uid(ret, 0);
      Store_field(ret, 1, TranslateNamedDecl(x));
      Store_field(ret, 2, TranslateQualType(x->getUnderlyingType()));
      Store_field(ret, 3, loc.TranslateSourceRange(org->getSourceRange()));
      Store_field(ret, 4, com.TranslateRawCommentOpt(Context->getRawCommentForDeclNoCache(org)));
    });
  CAMLreturn(ret);
}





/* Diagnostics */
/************* */


class MLDiagnostics : public DiagnosticConsumer {

private:

  typedef std::tuple<DiagnosticsEngine::Level, SourceLocation, std::string> diag;
  // Diagnostics are accumulated in this vector

  std::vector<diag> diags;
  MLLocationTranslator& loc;

public:
  MLDiagnostics(MLLocationTranslator& loc) : loc(loc)
  {}

  void HandleDiagnostic(DiagnosticsEngine::Level DiagLevel,
                        const Diagnostic &Info) override;

  CAMLprim value getDiagnostics();
  CAMLprim value TranslateDiagnostics(diag d);
};

void MLDiagnostics::HandleDiagnostic(DiagnosticsEngine::Level Level,
                                     const Diagnostic &Info) {
  // Default implementation (Warnings/errors count).
  DiagnosticConsumer::HandleDiagnostic(Level, Info);

  SmallString<256> Buf;
  Info.FormatDiagnostic(Buf);
  diags.emplace_back(Level, Info.getLocation(), Buf.str());
}


// Converts accumulated diagnostics to Ocaml list
CAMLprim value MLDiagnostics::getDiagnostics() {
  CAMLparam0();
  CAMLlocal1(ret);
  GENERATE_LIST(ret, diags, TranslateDiagnostics(child));
  CAMLreturn(ret);
}


enum {
  MLTAG_Level_Ignored,
  MLTAG_Level_Note,
  MLTAG_Level_Remark,
  MLTAG_Level_Warning,
  MLTAG_Level_Error,
  MLTAG_Level_Fatal,
};

CAMLprim value MLDiagnostics::TranslateDiagnostics(MLDiagnostics::diag d) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(3);
  int r;
  switch (std::get<0>(d)) {
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Ignored);
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Note);
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Remark);
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Warning);
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Error);
    GENERATE_CASE_PREFIX(r, DiagnosticsEngine::, Level_, Fatal);
  default:
    if (verbose_exn) { std::cout << "unknown diagnostics level : " << std::get<0>(d) << " " << std::get<2>(d) << std::endl; }
    caml_failwith("mlClangAST: unknown diagnostics level");
  }
  Store_field(ret, 0, Val_int(r));
  Store_field(ret, 1, loc.TranslateSourceLocation(std::get<1>(d)));
  Store_field(ret, 2, caml_copy_string((std::get<2>(d)).c_str()));

  CAMLreturn(ret);
}



/* Target options */
/* ************** */


/* string option -> std::string */
#define GET_STRING_OPTION(VAL)                                  \
  Is_block(VAL) ? std::string(String_val(Field(VAL,0))) : NULL

/* string list -> std::vector<std::string> stored into RES */
#define STORE_STRING_VECTOR(RES, VAL) {         \
    RES.clear();                                \
    value head_ = VAL;                          \
    while (Is_block(head_)) {                                   \
      RES.push_back(std::string(String_val(Field(head_,0))));   \
      head_ = Field(head_,1);                                   \
    }                                                           \
  }


enum {
  MLTAG_Target_EABI_Unknown,
  MLTAG_Target_EABI_Default,
  MLTAG_Target_EABI_EABI4,
  MLTAG_Target_EABI_EABI5,
  MLTAG_Target_EABI_GNU,
};

/* target_options -> TargetOptions */
CAMLprim void TargetOptionsFromML(value v, TargetOptions& t) {
  CAMLparam1(v);

  t.Triple = String_val(Field(v,0));
  t.HostTriple = String_val(Field(v,1));
  t.CPU = String_val(Field(v,2));
  t.FPMath = String_val(Field(v,3));
  t.ABI = String_val(Field(v,4));

#if CLANG_VERSION_MAJOR >= 5
  switch (Field(v,5)) {
    GENERATE_CASE_PREFIX_REV(t.EABIVersion, llvm::EABI::, Target_EABI_, Unknown);
    GENERATE_CASE_PREFIX_REV(t.EABIVersion, llvm::EABI::, Target_EABI_, Default);
    GENERATE_CASE_PREFIX_REV(t.EABIVersion, llvm::EABI::, Target_EABI_, EABI4);
    GENERATE_CASE_PREFIX_REV(t.EABIVersion, llvm::EABI::, Target_EABI_, EABI5);
    GENERATE_CASE_PREFIX_REV(t.EABIVersion, llvm::EABI::, Target_EABI_, GNU);
  default:
    t.EABIVersion = llvm::EABI::Default;
  }
#else
  t.EABIVersion = "";
#endif

  t.LinkerVersion = String_val(Field(v,6));
  STORE_STRING_VECTOR(t.FeaturesAsWritten, Field(v,7));
  STORE_STRING_VECTOR(t.Features, Field(v,8));
  /*STORE_STRING_VECTOR(t.Reciprocals, Field(v,9));*/ // Moved in later versions of Clang

  CAMLreturn0;
}

/* unit -> target_options */
CAMLprim value TargetOptionsToML(const TargetOptions& t) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(10);
  Store_field(ret, 0, caml_copy_string(t.Triple.c_str()));
  Store_field(ret, 1, caml_copy_string(t.HostTriple.c_str()));
  Store_field(ret, 2, caml_copy_string(t.CPU.c_str()));
  Store_field(ret, 3, caml_copy_string(t.FPMath.c_str()));
  Store_field(ret, 4, caml_copy_string(t.ABI.c_str()));

#if CLANG_VERSION_MAJOR >= 5
  {
    int i = MLTAG_Target_EABI_Default;
    switch (t.EABIVersion) {
      GENERATE_CASE_PREFIX(i, llvm::EABI::, Target_EABI_, Unknown);
      GENERATE_CASE_PREFIX(i, llvm::EABI::, Target_EABI_, Default);
      GENERATE_CASE_PREFIX(i, llvm::EABI::, Target_EABI_, EABI4);
      GENERATE_CASE_PREFIX(i, llvm::EABI::, Target_EABI_, EABI5);
      GENERATE_CASE_PREFIX(i, llvm::EABI::, Target_EABI_, GNU);
      //default:
      //if (verbose_exn) std::cout << "unknown EABI " << (int)t.EABIVersion << std::endl;
      //caml_failwith("mlClangAST: unknown EABI");
    }
    Store_field(ret, 5, Val_int(i));
  }
#else
  Store_field(ret, 5, Val_int(llvm::EABI::Default));
#endif

  Store_field(ret, 6, caml_copy_string(t.LinkerVersion.c_str()));
  Store_field_list(ret, 7, t.FeaturesAsWritten, caml_copy_string(child.c_str()));
  Store_field_list(ret, 8, t.Features, caml_copy_string(child.c_str()));
  /*Store_field_list(ret, 9, t.Reciprocals, caml_copy_string(child.c_str()));*/  // Moved in later versions of Clang

  CAMLreturn(ret);
}


/* unit -> default target_option */
CAML_EXPORT value mlclang_get_default_target_options(value unit) {
  CAMLparam1(unit);
  TargetOptions t;
  t.Triple = llvm::sys::getDefaultTargetTriple().c_str();
  t.EABIVersion = llvm::EABI::Default;
  CAMLreturn(TargetOptionsToML(t));
}

#define GENERATE_TARGET_INFO_INT_TYPE(T) Val_int(T)

CAMLprim value TargetInfoToML(const TargetInfo& t) {
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(44);
  Store_field(ret, 0, TargetOptionsToML(t.getTargetOpts()));

  // we only expose pointer info for address space 0
  Store_field(ret,  1, GENERATE_TARGET_INFO_INT_TYPE(t.getSizeType()));
  Store_field(ret,  2, GENERATE_TARGET_INFO_INT_TYPE(t.getIntMaxType()));
#if CLANG_VERSION_MAJOR >= 16
  Store_field(ret,  3, GENERATE_TARGET_INFO_INT_TYPE(t.getPtrDiffType(clang::LangAS::Default)));
#else
  Store_field(ret,  3, GENERATE_TARGET_INFO_INT_TYPE(t.getPtrDiffType(0)));
#endif
  Store_field(ret,  4, GENERATE_TARGET_INFO_INT_TYPE(t.getIntPtrType()));
  Store_field(ret,  5, GENERATE_TARGET_INFO_INT_TYPE(t.getWCharType()));
  Store_field(ret,  6, GENERATE_TARGET_INFO_INT_TYPE(t.getWIntType()));
  Store_field(ret,  7, GENERATE_TARGET_INFO_INT_TYPE(t.getChar16Type()));
  Store_field(ret,  8, GENERATE_TARGET_INFO_INT_TYPE(t.getChar32Type()));
  Store_field(ret,  9, GENERATE_TARGET_INFO_INT_TYPE(t.getInt64Type()));
  Store_field(ret, 10, GENERATE_TARGET_INFO_INT_TYPE(t.getSigAtomicType()));
  Store_field(ret, 11, GENERATE_TARGET_INFO_INT_TYPE(t.getProcessIDType()));

  // The width/align methods return a uint64_t, but the class attribute
  // show this data to be unsigned char! -> it will fit in an OCaml int
#if CLANG_VERSION_MAJOR >= 16
  Store_field(ret, 12, Val_int(t.getPointerWidth(clang::LangAS::Default)));
  Store_field(ret, 13, Val_int(t.getPointerAlign(clang::LangAS::Default)));
#else
  Store_field(ret, 12, Val_int(t.getPointerWidth(0)));
  Store_field(ret, 13, Val_int(t.getPointerAlign(0)));
#endif
  Store_field(ret, 14, Val_int(t.getBoolWidth()));
  Store_field(ret, 15, Val_int(t.getBoolAlign()));
  Store_field(ret, 16, Val_int(t.getCharWidth()));
  Store_field(ret, 17, Val_int(t.getCharAlign()));
  Store_field(ret, 18, Val_int(t.getShortWidth()));
  Store_field(ret, 19, Val_int(t.getShortAlign()));
  Store_field(ret, 20, Val_int(t.getIntWidth()));
  Store_field(ret, 21, Val_int(t.getIntAlign()));
  Store_field(ret, 22, Val_int(t.getLongWidth()));
  Store_field(ret, 23, Val_int(t.getLongAlign()));
  Store_field(ret, 24, Val_int(t.getLongLongWidth()));
  Store_field(ret, 25, Val_int(t.getLongLongAlign()));
  Store_field(ret, 26, Val_int(t.getHalfWidth()));
  Store_field(ret, 27, Val_int(t.getHalfAlign()));
  Store_field(ret, 28, Val_int(t.getFloatWidth()));
  Store_field(ret, 29, Val_int(t.getFloatAlign()));
  Store_field(ret, 30, Val_int(t.getDoubleWidth()));
  Store_field(ret, 31, Val_int(t.getDoubleAlign()));
  Store_field(ret, 32, Val_int(t.getLongDoubleWidth()));
  Store_field(ret, 33, Val_int(t.getLongDoubleAlign()));
  Store_field(ret, 34, Val_int(t.getFloat128Width()));
  Store_field(ret, 35, Val_int(t.getFloat128Align()));

  Store_field(ret, 36, Val_int(t.getLargeArrayMinWidth()));
  Store_field(ret, 37, Val_int(t.getLargeArrayAlign()));
  Store_field(ret, 38, Val_int(t.getSuitableAlign()));

  Store_field(ret, 39, Val_bool(t.isBigEndian()));
  Store_field(ret, 40, Val_bool(t.isTLSSupported()));
  Store_field(ret, 41, Val_bool(t.hasInt128Type()));
  Store_field(ret, 42, Val_bool(t.hasFloat128Type()));

#if CLANG_VERSION_MAJOR >= 6
  Store_field(ret, 43, caml_copy_int64(t.getNullPointerValue(clang::LangAS::Default)));
#else
  Store_field(ret, 43, caml_copy_int64(t.getNullPointerValue(0)));
#endif

  CAMLreturn(ret);
}


/* target_options -> target_info */
CAML_EXPORT value mlclang_get_target_info(value target) {
  CAMLparam1(target);
  CAMLlocal1(ret);

  CompilerInstance ci;
  ci.createDiagnostics();
  std::shared_ptr<TargetOptions> pto = std::make_shared<clang::TargetOptions>();
  TargetOptionsFromML(target, *pto);
  TargetInfo *pti = TargetInfo::CreateTargetInfo(ci.getDiagnostics(), pto);
  ret = TargetInfoToML(*pti);
  delete pti;

  CAMLreturn(ret);
}



/* Macro table */
/************* */


CAMLprim value getMacroTable(SourceManager& src, Preprocessor &pp, MLLocationTranslator& loc)
{
  CAMLparam0();
  CAMLlocal4(ret,tmp1,tmp2,tmp3);

  ret = Val_false;
  
  const IdentifierTable & tbl = pp.getIdentifierTable();
  for (auto& id : tbl) {
    const IdentifierInfo* i = id.getValue();
    if (i->hasMacroDefinition()) {
      MacroInfo* m = pp.getMacroInfo(i);

      GENERATE_LIST(tmp1, m->params(),
                    caml_copy_string(child->getName().str().c_str())
                    );

      GENERATE_LIST(tmp2, m->tokens(),
                    caml_copy_string((std::string(src.getCharacterData(child.getLocation()), child.getLength())).c_str())
                    );

      tmp3 = caml_alloc_tuple(4);
      Store_field(tmp3, 0, caml_copy_string(id.getKey().str().c_str()));
      Store_field(tmp3, 1, tmp1);
      Store_field(tmp3, 2, tmp2);
      Store_field(tmp3, 3, loc.TranslateSourceLocation(m->getDefinitionLoc()));

      tmp1 = caml_alloc_tuple(2);
      Store_field(tmp1, 0, tmp3);
      Store_field(tmp1, 1, ret);
      ret = tmp1;
    }
  }

  CAMLreturn(ret);
}


 
/* Source list */
/************** */


CAMLprim value getSources(SourceManager& src)
{
  CAMLparam0();
  CAMLlocal2(head,tmp);
  head = Val_unit;
  for (auto f = src.fileinfo_begin(); f != src.fileinfo_end(); f++) {
    tmp = caml_alloc_tuple(2);
#if CLANG_VERSION_MAJOR < 18
    const FileEntry& e = *f->first;
    Store_field(tmp, 0, caml_copy_string(e.getName().str().c_str()));
#else
    Store_field(tmp, 0, caml_copy_string(f->first.getName().str().c_str()));
#endif
    Store_field(tmp, 1, head);
    head = tmp;
  }
  CAMLreturn(head);
}

 

/* Parsing */
/* ******* */


class MLTreeBuilderConsumer : public ASTConsumer {
private:

  value* ret;
  MLLocationTranslator& loc;
  MLCommentTranslator& com;
  SourceManager& src;

public:
  explicit MLTreeBuilderConsumer(MLLocationTranslator& loc, value* ret, SourceManager& src, MLCommentTranslator& com)
    : ret(ret), loc(loc), com(com), src(src)
  {}

  virtual void HandleTranslationUnit(ASTContext &Context) {
    Decl* decl = Context.getTranslationUnitDecl();
    MLTreeBuilderVisitor Visitor(loc, &Context, src, com);
    *ret = Visitor.TranslateDecl(decl);
  }
};


CAML_EXPORT value mlclang_parse(value command, value target, value name, value args) {
  CAMLparam4(command,target,name,args);
  CAMLlocal2(ret,tmp);

  CompilerInstance ci;
  ci.createDiagnostics();

  // compiler command-line arguments
  std::vector<const char*> a;
  a.push_back(String_val(command));
  a.push_back("-c");
  a.push_back(String_val(name));
  for (size_t i = 0; i < Wosize_val(args); i++) {
    a.push_back(String_val(Field(args, i)));
  }
  std::shared_ptr<CompilerInvocation> invoke =
#if CLANG_VERSION_MAJOR >= 15
    std::move(createInvocation(a));
#else
    std::move(createInvocationFromCommandLine(a));
#endif
  if (!invoke) caml_failwith("mlClangAST: failed to create clang::CompilerInvocation");
  ci.setInvocation(invoke);

  // target
  std::shared_ptr<TargetOptions> pto = std::make_shared<clang::TargetOptions>();
  TargetOptionsFromML(target, *pto);
  TargetInfo *pti = TargetInfo::CreateTargetInfo(ci.getDiagnostics(), pto);
  ci.setTarget(pti);

  // source file
  ci.createFileManager();
  ci.createSourceManager(ci.getFileManager());
#if CLANG_VERSION_MAJOR < 10
  const FileEntry *pFile = ci.getFileManager().getFile(String_val(name));
#elif CLANG_VERSION_MAJOR < 18
  auto x = ci.getFileManager().getFile(String_val(name));
  if (!x) caml_failwith("mlClangAST: cannot get FileEntry");
  const FileEntry *pFile = x.get();
#else
  auto x = ci.getFileManager().getFileRef(String_val(name));
  if (!x) caml_failwith("mlClangAST: cannot get FileEntry");
  const FileEntryRef pFile = x.get();
#endif
  if (!pFile) caml_failwith("mlClangAST: cannot get FileEntry");
  SourceManager& src = ci.getSourceManager();
  src.setMainFileID(src.createFileID(pFile,SourceLocation(),SrcMgr::C_User));

  // headers
  // TODO: setting ResourceDir and calling GetResourcesPath does not seem to work
  // we use AddPath directly for now
  ci.getHeaderSearchOpts().AddPath(CLANGRESOURCE "/include",frontend::IncludeDirGroup::System, false, false);
  ci.getHeaderSearchOpts().UseBuiltinIncludes = false; //true;
  ci.getHeaderSearchOpts().UseStandardSystemIncludes = true;
  //ci.getHeaderSearchOpts().Verbose = true;

  // preprocessor
  ci.createPreprocessor(TU_Complete);
  Preprocessor &pp = ci.getPreprocessor();
  pp.getBuiltinInfo().initializeBuiltins(pp.getIdentifierTable(),
                                         pp.getLangOpts());

  // locations
  MLLocationTranslator loc(src, pp.getLangOpts());

  // custom diagnostics
  MLDiagnostics* diag = new MLDiagnostics(loc);
  MLCommentTranslator com(src, loc);
  ci.getDiagnostics().setClient(diag);

  // parsing
#if CLANG_VERSION_MAJOR < 10
  ci.setASTConsumer(llvm::make_unique<MLTreeBuilderConsumer>(loc, &tmp, src, com));
#else
  ci.setASTConsumer(std::make_unique<MLTreeBuilderConsumer>(loc, &tmp, src, com));
#endif
  ci.createASTContext();
  ci.getDiagnosticClient().BeginSourceFile(ci.getLangOpts(), &pp);
  ASTContext& Context = ci.getASTContext();
  ParseAST(pp, &ci.getASTConsumer(), Context);

  // get disgnostics
  ci.getDiagnosticClient().EndSourceFile();
    
  // return all info
  ret = caml_alloc_tuple(5);
  Store_field(ret, 0, tmp);
  Store_field(ret, 1, diag->getDiagnostics());
  Store_field(ret, 2, com.getRawCommentList(Context));
  Store_field(ret, 3, getMacroTable(src, pp, loc));
  Store_field(ret, 4, getSources(src));
    
  CAMLreturn(ret);
}
