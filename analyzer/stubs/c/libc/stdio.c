/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <stdio.h>
#include "mopsa_libc_utils.h"


/* Utilities */

static const int _EOF = EOF;
static const size_t _sizeof_FILE = sizeof(FILE);
static const size_t _BUFSIZ = BUFSIZ;
static const size_t _L_tmpnam = L_tmpnam;
static const size_t _L_ctermid = L_ctermid;

# if !defined __USE_XOPEN2K || defined __USE_GNU
static const size_t _L_cuserid = L_cuserid;
#endif


/*$
 * // Helper function
 * local:   int fd = new FileDescriptor;
 * local:   FILE* f = new File;
 * ensures: size(f) == _sizeof_FILE;
 * ensures: f->_fileno == fd;
 * ensures: return == f;
 */
static FILE* _alloc_FILE();



/* Stubs */


struct _IO_FILE *stdin;
struct _IO_FILE *stdout;
struct _IO_FILE *stderr;

/*$
 * // TODO: ensure that _stdio_init is called
 * local: void* _stdin  = _alloc_FILE();
 * local: void* _stdout = _alloc_FILE();
 * local: void* _stderr = _alloc_FILE();
 * ensures: stdin  == _stdin;
 * ensures: stdout == _stdout;
 * ensures: stderr == _stderr;
 * ensures: stdin ->_fileno == 0;
 * ensures: stdout->_fileno == 1;
 * ensures: stderr->_fileno == 2;
 */
static void _stdio_init();



const char *const sys_errlist[128]; // TODO: actual size

/*$
 * requires: exists int i in [0, size(__filename) - 1]: __filename[i] == 0;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int remove (const char *__filename);

/*$
 * requires: exists int i in [0, size(__old) - 1]: __old[i] == 0;
 * requires: exists int i in [0, size(__new) - 1]: __new[i] == 0;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int rename (const char *__old, const char *__new);

#ifdef __USE_ATFILE

/*$
 * requires: exists int i in [0, size(__old) - 1]: __old[i] == 0;
 * requires: exists int i in [0, size(__new) - 1]: __new[i] == 0;
 * requires: __oldfd in FileDescriptor;
 * requires: __newfd in FileDescriptor;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int renameat (int __oldfd, const char *__old, int __newfd, const char *__new);

#endif

/*$
 * case "success":
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *tmpfile (void);

static char _tmpnam_buf[L_tmpnam];

/*$
 * //warn: "avoid using this function";
 *
 * requires: __s != _NULL implies size(__s) >= _L_tmpnam;
 *
 * case "use_s":
 *   assumes: __s != _NULL;
 *   assigns: __s[0, _L_tmpnam - 1];
 *   ensures: exists int i in [0, _L_tmpnam - 1]: __s[i] == 0;
 *   ensures: return == __s;
 * 
 * case "use_static":
 *   assumes: __s == _NULL;
 *   assigns: _tmpnam_buf[0, size(_tmpnam_buf) - 1];
 *   ensures: exists int i in [0, size(_tmpnam_buf) - 1]: _tmpnam_buf[i] == 0;
 *   ensures: return == _tmpnam_buf;
 *
 * case "failure":
 *   ensures: return == _NULL;
 */
char *tmpnam (char *__s);

#ifdef __USE_MISC

/*$
 * //warn: "avoid using this function";
 *
 * requires: size(__s) >= _L_tmpnam;
 *
 * case "success":
 *   assigns: __s[0, _L_tmpnam - 1];
 *   ensures: exists int i in [0, _L_tmpnam - 1]: __s[i] == 0;
 *   ensures: return == __s;
 *
 * case "failure":
 *   ensures: return == _NULL;
 */
char *tmpnam_r (char *__s);

#endif


#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * //warn: "obsolete function, do not call"
 */
char *tempnam (const char *__dir, const char *__pfx);

#endif

/*$
 * requires: __stream in File;
 * //TODO: requires: __stream->_fileno in FileDescriptor;
 *
 * case "success":
 *   ensures:  free __stream->_fileno;
 *   ensures:  free __stream;
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _EOF;
 */
int fclose (FILE *__stream);

/*$
 * requires: __stream != _NULL implies __stream in File;
 * //TODO: requires: stream != _NULL implies__stream->_fileno in FileDescriptor;
 *
 * case "success":
 *   assumes:  __stream != _NULL;
 *   ensures:  free __stream->_fileno;
 *   ensures:  free __stream;
 *   ensures: return == 0;
 *
 * case "flushall":
 *   assumes: __stream == _NULL;
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _EOF;
 */
int fflush (FILE *__stream);

#ifdef __USE_MISC

/*$
 * local:   int r = fflush(__stream);
 * ensures: return == r;
 */
int fflush_unlocked (FILE *__stream);

#endif

// TODO: not implemented fcloseall

/*$
 * // TODO: validate characters in __modes
 *
 * requires: exists int i in [0, size(__filename) - 1]: __filename[i] == 0;
 * requires: exists int i in [0, size(__modes) - 1]: __modes[i] == 0;
 *
 * case "success":
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *fopen (const char *__restrict __filename,
             const char *__restrict __modes);

/*$
 * // TODO: validate characters in __modes
 *
 * requires: exists int i in [0, size(__filename) - 1]: __filename[i] == 0;
 * requires: exists int i in [0, size(__modes) - 1]: __modes[i] == 0;
 * requires: __stream in File;
 *
 * case "success":
 *   local:   int fd = new FileDescriptor;
 *   assigns: __stream->_fileno;
 *   ensures: __stream->_fileno == fd;
 *   ensures: return == __stream;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *freopen (const char *__restrict __filename,
               const char *__restrict __modes,
               FILE *__restrict __stream);

#ifdef	__USE_POSIX

/*$
 * requires: exists int i in [0, size(__modes) - 1]: __modes[i] == 0;
 * requires: __fd in FileDescriptor;
 *
 * case "success":
 *   local:   FILE* f = new File;
 *   ensures: size(f) == _sizeof_FILE;
 *   ensures: f->_fileno == __fd;
 *   ensures: return == f;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *fdopen (int __fd, const char *__modes);

#endif

// TODO: omitted __USE_GNU fopencookie

#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2)

/*$
 * // TODO: if __s != NULL, the buffer pointed to by __s becomes volatile
 *
 * requires: exists int i in [0, size(__modes) - 1]: __modes[i] == 0;
 * requires: __s != _NULL implies size(__s) >= __len;
 *
 * case "success":
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *fmemopen (void *__s, size_t __len, const char *__modes);

/*$
 * // TODO: *__bufloc and __sizeloc become volatile
 *
 * case "success":
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *open_memstream (char **__bufloc, size_t *__sizeloc);

#endif

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != _NULL implies size(__buf) >= _BUFSIZ;
 */
void setbuf (FILE *__restrict __stream, char *__restrict __buf);

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != _NULL implies size(__buf) >= __n;
 * requires: __modes >= 0 and __modes <= 2;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return != 0;
 */
int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
             int __modes, size_t __n);

#ifdef	__USE_MISC

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != _NULL implies size(__buf) >= __size;
 */
void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
                size_t __size);

/*$
 * requires: __stream in File;
 */
void setlinebuf (FILE *__stream);

#endif

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __stream in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int fprintf (FILE *__restrict __stream,
             const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments, check size of __s
 *
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int sprintf (char *__restrict __s,
             const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __s in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int vfprintf (FILE *__restrict __s, const char *__restrict __format,
              _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int vprintf (const char *__restrict __format, _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments, check size of __s
 *
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int vsprintf (char *__restrict __s, const char *__restrict __format,
              _G_va_list __arg);

#if defined __USE_ISOC99 || defined __USE_UNIX98

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: size(__s) >= __maxlen;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int snprintf (char *__restrict __s, size_t __maxlen,
              const char *__restrict __format, ...);;

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: size(__s) >= __maxlen;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
int vsnprintf (char *__restrict __s, size_t __maxlen,
               const char *__restrict __format, _G_va_list __arg);

#endif

#if __GLIBC_USE (LIB_EXT2)

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__f) - 1]: __f[i] == 0;
 *
 * case "success":
 *   local:    char* r = new Memory;
 *   assigns:  *__ptr;
 *   ensures:  *__ptr == r;
 *   ensures:  size(*__ptr) > 0;
 *   ensures:  exists int i in [0, size(*__ptr) - 1]: (*__ptr)[i] == 0;
 *   ensures:  return >= 0;
 *
 * case "error":
 *   assigns: *__ptr;
 *   ensures: return == -1;
 */
int vasprintf (char **__restrict __ptr, const char *__restrict __f,
               _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__fmt) - 1]: __fmt[i] == 0;
 *
 * case "success":
 *   local:    char* r = new Memory;
 *   assigns:  *__ptr;
 *   ensures:  *__ptr == r;
 *   ensures:  size(*__ptr) > 0;
 *   ensures:  exists int i in [0, size(*__ptr) - 1]: (*__ptr)[i] == 0;
 *   ensures:  return >= 0;
 *
 * case "error":
 *   assigns: *__ptr;
 *   ensures: return == -1;
 */
int asprintf (char **__restrict __ptr,
              const char *__restrict __fmt, ...);

#endif

#ifdef __USE_XOPEN2K8

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __fd in FileDescriptor;
 * requires: exists int i in [0, size(__fmt) - 1]: __fmt[i] == 0;
 */
int vdprintf (int __fd, const char *__restrict __fmt,
              _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __fd in FileDescriptor;
 * requires: exists int i in [0, size(__fmt) - 1]: __fmt[i] == 0;
 */
int dprintf (int __fd, const char *__restrict __fmt, ...);

#endif

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __stream in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int fscanf (FILE *__restrict __stream,
            const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int scanf (const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int sscanf (const char *__restrict __s,
            const char *__restrict __format, ...);


#ifdef	__USE_ISOC99

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __s in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int vfscanf (FILE *__restrict __s, const char *__restrict __format,
             _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int vscanf (const char *__restrict __format, _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
int vsscanf (const char *__restrict __s,
             const char *__restrict __format, _G_va_list __arg);

#endif /* Use ISO C9x.  */


/*$
 * requires: __stream in File;
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int fgetc (FILE *__stream);

/*$
 * requires: __stream in File;
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int getc (_IO_FILE *__stream);

/*$
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int getchar (void);

#ifdef __USE_POSIX199506

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int getc_unlocked (FILE *__stream);

/*$
 * // TODO: not thread-safe
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int getchar_unlocked (void);

#endif /* Use POSIX.  */

#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return >= 0 and return <= 255) or return == _EOF;
 */
int fgetc_unlocked (FILE *__stream);

#endif /* Use MISC.  */

/*$
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int fputc (int __c, FILE *__stream);

/*$
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int putc (int __c, _IO_FILE *__stream);

/*$
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int putchar (int __c);

#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int fputc_unlocked (int __c, FILE *__stream);

#endif /* Use MISC.  */

#ifdef __USE_POSIX199506

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int putc_unlocked (int __c, FILE *__stream);

/*$
 * // TODO: not thread-safe
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int putchar_unlocked (int __c);

#endif /* Use POSIX.  */


#if defined __USE_MISC || (defined __USE_XOPEN && !defined __USE_XOPEN2K)

/*$
 * requires: __stream in File;
 */
int getw (FILE *__stream);

/*$
 * requires: __stream in File;
 * ensures:  (return == 0) or (return == _EOF);
 */
int putw (int __w, FILE *__stream);

#endif

/*$
 * requires: __stream in File;
 * requires:  size(__s) >= __n;
 * assigns:   __s[0, size(__s) - 1];
 * ensures:   exists int i in [0, size(__s) - 1]: __s[i] == 0;
 * ensures:   (return == __s) or (return == _NULL);
 */
char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);

/*$
 * //warn: "dangerous function, do not use";
 * ensures: (return == __s) or (return == _NULL);
 */
char *gets (char *__s);

#ifdef __USE_GNU

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires:  size(__s) >= __n;
 * assigns:   __s[0, size(__s) - 1];
 * ensures:   exists int i in [0, size(__s) - 1]: __s[i] == 0;
 * ensures:   (return == __s) or (return == _NULL);
 */
char *fgets_unlocked (char *__restrict __s, int __n,
                      FILE *__restrict __stream);

#endif


#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2)

/*$
 * requires: __stream in File;
 * requires: *__lineptr != _NULL implies size(*__lineptr) >= *__n;
 * 
 * case "realloc":
 *   assumes:  *__lineptr != _NULL;
 * //TODO:   requires: *__lineptr in Memory;
 *   local:   char* r = new Memory;
 *   assigns: *__lineptr;
 *   assigns: *__n;
 *   ensures: free old(*__lineptr);
 *   ensures: size(*__lineptr) == *__n;
 *   ensures: *__lineptr == r;
 *   ensures: return >= 0;
 *
 * case "alloc":
 *   assumes: *__lineptr == _NULL;
 *   local:   char* r = new Memory;
 *   assigns: *__lineptr;
 *   assigns: *__n;
 *   ensures: size(*__lineptr) == *__n;
 *   ensures: *__lineptr == r;
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
ssize_t getdelim (char **__restrict __lineptr,
                  size_t *__restrict __n, int __delimiter,
                  FILE *__restrict __stream);

/*$
 * local: ssize_t r  = getdelim(__lineptr, __n, 10, __stream);
 * ensures: return == r;
 */
ssize_t getline (char **__restrict __lineptr,
                 size_t *__restrict __n,
                 FILE *__restrict __stream);

#endif

/*$
 * requires: __stream in File;
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "success":
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _EOF;
 */
int fputs (const char *__restrict __s, FILE *__restrict __stream);

/*$
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "success":
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _EOF;
 */
int puts (const char *__s);

/*$
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == _EOF);
 */
int ungetc (int __c, FILE *__stream);

/*$
 * requires: __stream in File;
 * requires: size(__ptr) >= __size * __n;
 * assigns:  __ptr[0, __size * __n - 1];
 * ensures:  return >= 0 and return <= __n;
 */
size_t fread (void *__restrict __ptr, size_t __size,
              size_t __n, FILE *__restrict __stream);

/*$
 * requires: __s in File;
 * requires: size(__ptr) >= __size * __n;
 * ensures:  return >= 0 and return <= __n;
 */
size_t fwrite (const void *__restrict __ptr, size_t __size,
               size_t __n, FILE *__restrict __s);

#ifdef __USE_GNU

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "success":
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _EOF;
 */
int fputs_unlocked (const char *__restrict __s,
                    FILE *__restrict __stream);

#endif

#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: size(__ptr) >= __size * __n;
 * assigns:  __ptr[0, __size * __n - 1];
 * ensures:  return >= 0 and return <= __n;
 */
size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                       size_t __n, FILE *__restrict __stream);

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: size(__ptr) >= __size * __n;
 * ensures:  return >= 0 and return <= __n;
 */
size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
                        size_t __n, FILE *__restrict __stream);

#endif

/*$
 * requires: __stream in File;
 * requires: __whence >= 0 and __whence <= 2;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int fseek (FILE *__stream, long int __off, int __whence);

/*$
 * requires: __stream in File;
 *
 * case "success":
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
long int ftell (FILE *__stream);

/*$
 * requires: __stream in File;
 */
void rewind (FILE *__stream);

#if defined __USE_LARGEFILE || defined __USE_XOPEN2K

/*$
 * requires: __stream in File;
 * requires: __whence >= 0 and __whence <= 2;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int fseeko (FILE *__stream, __off_t __off, int __whence);

/*$
 * requires: __stream in File;
 *
 * case "success":
 *   ensures: return >= 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
__off_t ftello (FILE *__stream);

#endif

/*$
 * requires: __stream in File;
 *
 * case "success":
 *   assigns: *__pos;
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);

/*$
 * requires: __stream in File;
 * // TODO: requires: size(__pos) >= sizeof(fpos_t);
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int fsetpos (FILE *__stream, const fpos_t *__pos);

/*$
 * requires: __stream in File;
 */
void clearerr (FILE *__stream);

/*$
 * requires: __stream in File;
 */
int feof (FILE *__stream);

/*$
 * requires: __stream in File;
 */
int ferror (FILE *__stream);

#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 */
void clearerr_unlocked (FILE *__stream);

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 */
int feof_unlocked (FILE *__stream);

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 */
int ferror_unlocked (FILE *__stream);

#endif

/*$
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 */
void perror (const char *__s);

#ifdef	__USE_POSIX

/*$
 * requires: __stream in File;
 * ensures:  return == __stream->_fileno;
 */
int fileno (FILE *__stream);

#endif /* Use POSIX.  */

#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures:  return == __stream->_fileno;
 */
int fileno_unlocked (FILE *__stream);

#endif


#ifdef __USE_POSIX2

/*$
 * requires: exists int i in [0, size(__command) - 1]: __command[i] == 0;
 * requires: exists int i in [0, size(__modes) - 1]: __modes[i] == 0;
 *
 * case "success":
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
FILE *popen (const char *__command, const char *__modes);

/*$
 * requires: __stream in File;
 * assigns:  _errno;
 * ensures:  free __stream;
 */
int pclose (FILE *__stream);

#endif


#ifdef	__USE_POSIX

static char _ctermid_buf[L_ctermid];

/*$
 * requires: __s != _NULL implies size(__s) >= _L_ctermid;
 *
 * case "buf":
 *   assumes: __s != _NULL;
 *   assigns: __s[0, _L_ctermid - 1];
 *   ensures: exists int i in [0, _L_ctermid - 1]: __s[i] == 0;
 *   ensures: return == __s;
 *
 * case "nobuf":
 *   assumes: __s == _NULL;
 *   assigns: _ctermid_buf[0, _L_ctermid - 1];
 *   ensures: exists int i in [0, _L_ctermid - 1]: _ctermid_buf[i] == 0;
 *   ensures: return == &_ctermid_buf[0];
 */
char *ctermid (char *__s);

#endif /* Use POSIX.  */


#if (defined __USE_XOPEN && !defined __USE_XOPEN2K) || defined __USE_GNU

static char _cuserid_buf[L_cuserid];

/*$
 * requires: __s != _NULL implies size(__s) >= _L_cuserid;
 *
 * case "buf":
 *   assumes: __s != _NULL;
 *   assigns: __s[0, _L_cuserid - 1];
 *   ensures: exists int i in [0, _L_cuserid - 1]: __s[i] == 0;
 *   ensures: return == __s;
 *
 * case "nobuf":
 *   assumes: __s == _NULL;
 *   assigns: _cuserid_buf[0, _L_cuserid - 1];
 *   ensures: exists int i in [0, _L_cuserid - 1]: _cuserid_buf[i] == 0;
 *   ensures: return == &_cuserid_buf[0];
 */
char *cuserid (char *__s);

#endif /* Use X/Open, but not issue 6.  */


// omitted: obstack with __USE_GNU

#ifdef __USE_POSIX199506

/*$
 * requires: __stream in File;
 */
void flockfile (FILE *__stream);

/*$
 * requires: __stream in File;
 */
int ftrylockfile (FILE *__stream);

/*$
 * requires: __stream in File;
 */
void funlockfile (FILE *__stream);

#endif /* POSIX */
