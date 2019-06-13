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
  libc stub
  based on header from glibc-2.27-r6
*/
#include <stdlib.h>
#include <limits.h>
#include "mopsa_libc_utils.h"


/*$
 * local: char* r = new Memory;
 * ensures: return == r;
 * ensures: size(return) in [1, max];
 * ensures: return[size(return) - 1] == 0;
 */
static char* _alloc_string(size_t max);


/* Stubs */


/*$
 * ensures: return in [1, MB_LEN_MAX];
 */
size_t __ctype_get_mb_cur_max (void);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 */
double atof (const char *__nptr);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 */
int atoi (const char *__nptr);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 */
long int atol (const char *__nptr);

#ifdef __USE_ISOC99

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 */
long long int atoll (const char *__nptr);

#endif

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
double strtod (const char *__restrict __nptr,
               char **__restrict __endptr);

#ifdef	__USE_ISOC99

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
float strtof (const char *__restrict __nptr,
              char **__restrict __endptr);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
long double strtold (const char *__restrict __nptr,
                     char **__restrict __endptr);

#endif

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
long int strtol (const char *__restrict __nptr,
                 char **__restrict __endptr, int __base);


/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
unsigned long int strtoul (const char *__restrict __nptr,
                           char **__restrict __endptr, int __base);


#ifdef __USE_MISC

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
long long int strtoq (const char *__restrict __nptr,
                      char **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
unsigned long long int strtouq (const char *__restrict __nptr,
                                char **__restrict __endptr, int __base);

#endif

#ifdef __USE_ISOC99

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: valid_string(__nptr);
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
long long int strtoll (const char *__restrict __nptr,
                       char **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 * requires: __base == 0 or __base in [2, 36];
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  (*__endptr)' >= __nptr and (*__endptr)' < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
unsigned long long int strtoull (const char *__restrict __nptr,
                                 char **__restrict __endptr, int __base);

#endif



// omitted: __USE_GNU functions


#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

static char _l64a_buf[7];

/*$
 * // TODO: return one-use allocated string?
 * requires: __n >= 0;
 * assigns:  _l64a_buf[0, 6];
 * ensures:  return == _l64a_buf;
 * ensures:  valid_primed_string(_l64a_buf); 
 */
char *l64a (long int __n);

/*$
 * requires: size(__s) >= 6 or valid_substring(__s, 6);
 */
long int a64l (const char *__s);

#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * ensures: return in [0, RAND_MAX];
 */
long int random (void);

/*$
 * // empty contract
 */
void srandom (unsigned int __seed);

static char _rand__state[128];
static char* _rand__statebuf = _rand__state;
static size_t _rand__statelen = sizeof(_rand__statebuf);

/*$
 * // TODO: return NULL when __statelen < 8?
 * requires: size(__statebuf) >= __statelen and __statelen >= 8;
 * assigns:  _rand__statebuf;
 * assigns:  _rand__statelen;
 * assigns:  __statebuf[0, __statelen - 1];
 * ensures:  _rand__statebuf' == __statebuf and _rand__statelen' == __statelen;
 * ensures:  return == _rand__statebuf;
 */
char *initstate (unsigned int __seed, char *__statebuf, size_t __statelen);

/*$
 * requires: size(__statebuf) >= _rand__statelen;
 * assigns:  _rand__statebuf;
 * ensures:  _rand__statebuf' == __statebuf;
 * ensures:  return == _rand__statebuf;
 */
char *setstate (char *__statebuf);

#endif

#ifdef __USE_MISC

/*$
 * assigns: *__buf;
 * assigns: *__result;
 * assigns: _errno;
 * ensures: (*__result)' in [0, RAND_MAX]; 
 * ensures: return in [-1, 0];
 */
int random_r (struct random_data *__restrict __buf,
              int32_t *__restrict __result);

/*$
 * assigns: *__buf;
 * assigns: _errno;
 * ensures: return in [-1, 0];
 */
int srandom_r (unsigned int __seed, struct random_data *__buf);

/*$
 * requires: __statelen >= 8;
 * requires: size(__statebuf) >= __statelen;
 * assigns:  *__statebuf[0, __statelen - 1];
 * assigns:  *__buf;
 * assigns:  _errno;
 * ensures:  return in [-1, 0];
 * ensures:  (__buf->rand_deg)' == __statelen; // keep length information around
 */
int initstate_r (unsigned int __seed, char *__restrict __statebuf,
                 size_t __statelen,
                 struct random_data *__restrict __buf);

/*$
 * requires: size(__statebuf) >= __buf->rand_deg;
 * assigns:  *__statebuf[0, __buf->rand_deg - 1];
 * assigns:  *__buf;
 * assigns:  _errno;
 * ensures:  return in [-1, 0];
 * ensures:  (__buf->rand_deg)' == __buf->rand_deg; // keep length information around
 */
int setstate_r (char *__restrict __statebuf,
                struct random_data *__restrict __buf);

#endif

/*$
 * ensures: return in [0, RAND_MAX];
 */
int rand (void);

/*$
 * // empty contract
 */
void srand (unsigned int __seed);

#ifdef __USE_POSIX199506

/*$
 * assigns: *__seed;
 * ensures: return in [0, RAND_MAX];
 */
int rand_r (unsigned int *__seed);

#endif

#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * ensures: valid_float(return) and return in [0., 1.];
 */
double drand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: valid_float(return) and return in [0., 1.];
 */
double erand48 (unsigned short int __xsubi[3]);

/*$
 * ensures: return in [0, 2147483647];
 */
long int lrand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: return >= 0 and return <= 2147483647;
 */
long int nrand48 (unsigned short int __xsubi[3]);

/*$
 * ensures: return in [-2147483648, 2147483647];
 */
long int mrand48 (void);

/*$
 * assigns: __xsubi[0, 2];
 * ensures: return in [-2147483648, 2147483647];
 */
long int jrand48 (unsigned short int __xsubi[3]);

/*$
 * // empty contract
 */
void srand48 (long int __seedval);

static unsigned short int _seed48_buf[3];

/*$
 * requires: size(&__seed16v[0]) >= 3;
 * ensures:  return == &_seed48_buf[0];
 */
unsigned short int *seed48 (unsigned short int __seed16v[3]);

/*$
 * requires: size(&__param[0]) >= 7;
 */
void lcong48 (unsigned short int __param[7]);

#endif

# ifdef __USE_MISC

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: valid_float((*__result)') and (*__result)' in [0., 1.];
 * ensures: return == 0;
 */
int drand48_r (struct drand48_data *__restrict __buffer,
               double *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: valid_float((*__result)') and (*__result)' in [0., 1.];
 * ensures: return == 0;
 */
int erand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               double *__restrict __result);
/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: (*__result)' in [0, 2147483647];
 * ensures: return == 0;
 */
int lrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: (*__result)' in [0, 2147483647];
 * ensures: return == 0;
 */
int nrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * ensures: (*__result)' in [-2147483648, 2147483647];
 * ensures: return == 0;
 */
int mrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__result;
 * assigns: *__buffer;
 * assigns: __xsubi[0, 2];
 * ensures: (*__result)' in [-2147483648, 2147483647];
 * ensures: return == 0;
 */
int jrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

/*$
 * assigns: *__buffer;
 * ensures: return == 0;
 */
int srand48_r (long int __seedval, struct drand48_data *__buffer);

/*$
 * requires: size(&__seed16v[0]) >= 3;
 * assigns:  *__buffer;
 * ensures:  return == 0;
 */
int seed48_r (unsigned short int __seed16v[3],
              struct drand48_data *__buffer);

/*$
 * requires: size(&__param[0]) >= 7;
 * assigns:  *__buffer;
 * ensures:  return == 0;
 */
int lcong48_r (unsigned short int __param[7],
               struct drand48_data *__buffer);

#endif


/*$
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size; // __size==0 also possible here
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   ensures: return == NULL;
 * }
 */
void *malloc (size_t __size);

/*$
 * //TODO: check overflow in __nmemb * __size
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __nmemb * __size;
 *   ensures: forall int i in [0, __nmemb * __size - 1]: ((char*)r)[i] == 0;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 *
 * case "empty" {
 *   assumes: __nmemb == 0 or __size == 0;
 *   ensures: return == NULL;
 * }
 */
void *calloc (size_t __nmemb, size_t __size);

/*$
 * case "nop" {
 *   assumes:  __ptr == NULL;
 *   assumes:  __size == 0;
 * }
 *
 * case "alloc" {
 *   assumes:  __ptr == NULL;
 *   local:    void* r = new Memory;
 *   ensures:  size(r) == __size; // __size==0 also possible here
 *   ensures:  return == r;
 * }
 *
 * case "free" {
 *   assumes:  __ptr != NULL;
 *   assumes:  __size == 0;
 *   requires: __ptr in Memory;
 *   free:     __ptr;
 * }
 *
 * case "resize" {
 *   assumes:  __ptr != NULL;
 *   assumes:  __size > 0;
 *   requires: __ptr in Memory;
 *   local:    void* r = new Memory;
 *   ensures:  size(r) == __size;
 *   ensures:  size(__ptr) >= __size implies 
 *             forall int i in [0, __size - 1]: (char*)r[i] == (char*)__ptr[i];
 *   ensures:  size(__ptr) <= __size implies 
 *             forall int i in [0, size(__ptr) - 1]: (char*)r[i] == (char*)__ptr[i];
 *   free:     __ptr;
 *   ensures:  return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
void *realloc (void *__ptr, size_t __size);

// omitted: __USE_GNU reallocarray

/*$
 * case "nop" {
 *   assumes:  __ptr == NULL;
 * }
 *
 * case "free" {
 *   assumes:  __ptr != NULL;
 *   requires: __ptr in Memory;
 *   free:     __ptr;
 * }
 */
void free (void *__ptr);

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K) || defined __USE_MISC

/*$
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
void *valloc (size_t __size);

#endif

#ifdef __USE_XOPEN2K

static const size_t _sizeof_ptr = sizeof(void*);

/*$
 * requires: __alignment % _sizeof_ptr == 0;
 * requires: __alignment & (__alignment - 1) == 0; // __alignment is a power of 2
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   assigns: **__memptr;
 *   ensures: size(r) == __size;
 *   ensures: (*__memptr)' == r;
 *   ensures: return == 0;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   assigns: **__memptr;
 *   ensures: (*__memptr)' == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   ensures: return != 0;
 * }
 */
int posix_memalign (void **__memptr, size_t __alignment, size_t __size);

#endif

#ifdef __USE_ISOC11

/*$
 * requires: __alignment & (__alignment - 1) == 0; // __alignment is a power of 2
 * requires: __size % __alignment == 0;
 *
 * case "success" {
 *   local:   void* r = new Memory;
 *   ensures: size(r) == __size;
 *   ensures: return == r;
 * }
 *
 * case "empty" {
 *   assumes: __size == 0;
 *   ensures: return == NULL;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
void *aligned_alloc (size_t __alignment, size_t __size);

#endif

/*$
 * ensures: 1 == 0;
 */
void abort (void);

/*$
 * warn: "unsupported stub atexit";
 */
int atexit (void (*__func) (void));

#if defined __USE_ISOC11 || defined __USE_ISOCXX11

/*$
 * warn: "unsupported stub";
 */
int at_quick_exit (void (*__func) (void)) ;

#endif

#ifdef	__USE_MISC

/*$
 * warn: "unsupported stub";
 */
int on_exit (void (*__func) (int __status, void *__arg), void *__arg);

#endif

/*$
 * ensures: 1 == 0;
 */
void exit (int __status);

#if defined __USE_ISOC11 || defined __USE_ISOCXX11

/*$
 * ensures: 1 == 0;
 */
void quick_exit (int __status);

#endif

#ifdef __USE_ISOC99

/*$
 * ensures: 1 == 0;
 */
void _Exit (int __status);

#endif

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = new ReadOnlyMemory;
 *   ensures:  return == r;
 *   ensures:  valid_string(return);
 * }
 *
 * case "failure" {
 *   ensures:  return == NULL;
 * }
 */
char *getenv (const char *__name);

#ifdef __USE_GNU

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = new ReadOnlyMemory;
 *   ensures:  return == r;
 *   ensures:  valid_string(return);
 * }
 *
 * case "failure" {
 *   ensures:  return == NULL;
 * }
 */
char *secure_getenv (const char *__name);

#endif

#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * //TODO: the function takes ownership of the string
 *
 * requires: valid_string(__string);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return != 0;
 * }
 */
int putenv (char *__string);

#endif

#ifdef __USE_XOPEN2K

/*$
 * requires: valid_string(__name);
 * requires: valid_string(__value);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == -1;
 * }
 */
int setenv (const char *__name, const char *__value, int __replace);

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures:  return == 0;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == -1;
 * }
 */
int unsetenv (const char *__name);

#endif

#ifdef	__USE_MISC

/*$
 * // empty contract
 */
int clearenv (void);

#endif

#if defined __USE_MISC || (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8)

/*$
 * // TODO: the last 6 chars of __template must be X
 * // TODO: only the last 6 chars of __template are modified, not the whole block
 * warn: "dangerous function, do not use";
 * requires: valid_string(__template);
 * assigns:  __template[0, size(__template) - 1];
 * assigns:  _errno;
 * ensures:  return == __template;
 */
char *mktemp (char *__template);

#endif

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * // TODO: same issue as mktemp
 * requires: valid_string(__template);
 *
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   assigns: __template[0, size(__template) - 1];
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: __template[0, size(__template) - 1];
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkstemp (char *__template);

#endif

#ifdef __USE_MISC

/*$
 * // TODO: same issue as mktemp
 * requires: valid_string(__template);
 * requires: __suffixlen >= 0;
 *
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   assigns: __template[0, size(__template) - 1];
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: __template[0, size(__template) - 1];
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkstemps (char *__template, int __suffixlen);

#endif
  
#ifdef __USE_XOPEN2K8
  
/*$
 * // TODO: same issue as mktemp
 * requires: valid_string(__template);
 *
 * case "success" {
 *   assigns:  __template[0, size(__template) - 1];
 *   ensures:  return == __template;
 * }
 *
 * case "failure" {
 *   assigns:  __template[0, size(__template) - 1];
 *   assigns:  _errno;
 *   ensures:  return == NULL;
 * }
 */
char *mkdtemp (char *__template);

#endif

#ifdef __USE_GNU

/*$
 * // TODO: same issue as mktemp
 * requires: valid_string(__template);
 *
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   assigns: __template[0, size(__template) - 1];
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: __template[0, size(__template) - 1];
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkostemp (char *__template, int __flags);

/*$
 * // TODO: same issue as mktemp
 * requires: valid_string(__template);
 * requires: __suffixlen >= 0;
 *
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   assigns: __template[0, size(__template) - 1];
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: __template[0, size(__template) - 1];
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int mkostemps (char *__template, int __suffixlen, int __flags);

#endif

/*$
 * requires: valid_string(__command);
 * warn: "unsupported stub";
 */
int system (const char *__command);


#ifdef	__USE_GNU

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   local:    char* r = _alloc_string(PATH_MAX);
 *   ensures:  return == r;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == NULL;
 * }
 */
char *canonicalize_file_name (const char *__name);

#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * requires: valid_string(__name);
 *
 * case "alloc" {
 *   assumes:  __resolved == NULL;
 *   local:    char* r = _alloc_string(PATH_MAX);
 *   ensures:  return == r;
 * }
 *
 * case "copy" {
 *   assumes:  __resolved != NULL;
 *   assigns:  __resolved[0, PATH_MAX - 1];
 *   ensures:  valid_primed_substring(__resolved, PATH_MAX);
 *   ensures:  return == __resolved;
 * }
 *
 * case "failure" {
 *   assigns:  _errno;
 *   ensures:  return == NULL;
 * }
 */
char *realpath (const char *__restrict __name,
                char *__restrict __resolved);

#endif

/*$
 * warn: "unsupported stub";
 */
void *bsearch (const void *__key, const void *__base,
               size_t __nmemb, size_t __size, __compar_fn_t __compar);

/*$
 * warn: "unsupported stub";
 */
void qsort (void *__base, size_t __nmemb, size_t __size,
            __compar_fn_t __compar);

#ifdef __USE_GNU

/*$
 * warn: "unsupported stub";
 */
void qsort_r (void *__base, size_t __nmemb, size_t __size,
              __compar_d_fn_t __compar, void *__arg);

#endif


/*$
 * requires: __x > INT_MAX;
 * ensures:  (__x >= 0 implies return == __x) and
 *           (__x < 0 implies return == -__x);
 */
int abs (int __x);

/*$
 * requires: __x > LONG_MAX;
 * ensures:  (__x >= 0 implies return == __x) and
 *           (__x < 0 implies return == -__x);
 */
long int labs (long int __x);

#ifdef __USE_ISOC99

/*$
 * requires: __x > LLONG_MAX;
 * ensures:  (__x >= 0 implies return == __x) and
 *           (__x < 0 implies return == -__x);
 */
long long int llabs (long long int __x);

#endif

/*$
 * requires: __denom != 0;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
div_t div (int __numer, int __denom);

/*$
 * requires: __denom != 0;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
ldiv_t ldiv (long int __numer, long int __denom);

#ifdef __USE_ISOC99

/*$
 * requires: __denom != 0;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
lldiv_t lldiv (long long int __numer, long long int __denom);

#endif

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) || defined __USE_MISC

/*$
 * warn: "obsolete function, do not call";
 */
char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

/*$
 * warn: "obsolete function, do not call";
 */
char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

/*$
 * warn: "obsolete function, do not call";
 */
char *gcvt (double __value, int __ndigit, char *__buf);

#endif

#ifdef __USE_MISC

/*$
 * warn: "obsolete function, do not call";
 */
char *qecvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

/*$
 * warn: "obsolete function, do not call";
 */
char *qfcvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

/*$
 * warn: "obsolete function, do not call";
 */
char *qgcvt (long double __value, int __ndigit, char *__buf);

/*$
 * warn: "obsolete function, do not call";
 */
int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

/*$
 * warn: "obsolete function, do not call";
 */
int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

/*$
 * warn: "obsolete function, do not call";
 */
int qecvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

/*$
 * warn: "obsolete function, do not call";
 */
int qfcvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

#endif


/*$
 * requires: __s != NULL implies size(__s) >= __n;
 *
 * case "shift" {
 *   assumes:  __s != NULL;
 *   ensures: return in [-1, __n];
 * }
 *
 * case "reset" {
 *   assumes:  __s == NULL;
 * }
 */
int mblen (const char *__s, size_t __n);

/*$
 * requires: __s != NULL implies size(__s) >= __n;
 *
 * case "shift" {
 *   assumes:  __s != NULL and __pwc == NULL;
 *   ensures: return in [-1, __n];
 * }
 *
 * case "copy" {
 *   assumes:  __s != NULL and __pwc != NULL;
 *   assigns:  *__pwc;
 *   ensures:  return in [-1, __n];
 * }
 *
 * case "reset" {
 *   assumes:  __s == NULL;
 * }
 */
int mbtowc (wchar_t *__restrict __pwc,
            const char *__restrict __s, size_t __n);

/*$
 * requires: __s != NULL implies size(__s) >= MB_LEN_MAX;
 *
 * case "shift" {
 *   assumes: __s != NULL; 
 *   assigns: __s[0, MB_LEN_MAX - 1];
 *   ensures: return in [-1, MB_LEN_MAX];
 * }
 *
 * case "reset" {
 *   assumes: __s == NULL;
 * }
 */
int wctomb (char *__s, wchar_t __wchar);

/*$
 * requires: __dst != NULL implies size(__dst) >= __len;
 * requires: valid_string(__src);
 *
 * case "copy" {
 *   assumes: __dst != NULL;
 *   assigns: __dst[0, __len - 1];
 * }
 *
 * case "count" {
 *   assumes: __dst == NULL;
 * }
 *
 * // can return (size_t)-1, or a value less than __len
 */
size_t mbstowcs (wchar_t *__restrict  __dst,
                 const char *__restrict __src, size_t __len);

/*$
 * requires: __dst != NULL implies size(__dst) >= __len;
 * requires: valid_string(__src);
 *
 * case "copy" {
 *   assumes: __dst != NULL;
 *   assigns: __dst[0, __len - 1];
 * }
 *
 * case "count" {
 *   assumes: __dst == NULL;
 * }
 *
 * // can return (size_t)-1, or a value less than __len
 */
size_t wcstombs (char *__restrict __dst,
                 const wchar_t *__restrict __src, size_t __len);

#ifdef __USE_MISC

/*$
 * requires: valid_string(__response);
 * ensures:  return in [-1, 1];
 */
int rpmatch (const char *__response);

#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * warn: "unsupported stub";
 */
int getsubopt (char **__restrict __optionp,
               char *const *__restrict __tokens,
               char **__restrict __valuep);

#endif


#ifdef __USE_XOPEN

/*$
 * requires: size(__key) >= 64;
 */
void setkey (const char *__key);

#endif


#ifdef __USE_XOPEN2KXSI

/*$
 * case "success" {
 *   local:   in fd = new FileDescriptor;
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int posix_openpt (int __oflag);

#endif

#ifdef __USE_XOPEN_EXTENDED

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int grantpt (int __fd);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int unlockpt (int __fd);

static char _ptsname_buf[1024]; // TODO: fix size

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   assigns: _ptsname_buf[0, size(_ptsname_buf) - 1];
 *   ensures: valid_primed_string(_ptsname_buf);
 *   ensures: return == (char*)_ptsname_buf;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *ptsname (int __fd) ;

#endif

#ifdef __USE_GNU

/*$
 * requires: __fd in FileDescriptor;
 * requires: size(__buf) >= __buflen;
 *
 * case "success" {
 *   assigns: __buf[0, __buflen - 1];
 *   ensures: valid_primd_substring(__buf, __buflen);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return != 0;
 * }
 */
int ptsname_r (int __fd, char *__buf, size_t __buflen);

/*$
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getpt (void);

#endif

#ifdef __USE_MISC

/*$
 * requires: size(__loadavg) >= __nelem;
 * assigns:  __loadavg[0, __nelem - 1];
 * ensures:  return in [-1, __nelem];
 */
int getloadavg (double __loadavg[], int __nelem);

#endif

