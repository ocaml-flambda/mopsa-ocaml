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
#include <stdarg.h>
#include <stdio.h>
#include "mopsa_libc_utils.h"


/*$
 * local:   struct _IO_FILE* addr = new File;
 * ensures: stdin == addr;
 * ensures: stdin->_fileno == 0;
 */
struct _IO_FILE *stdin;

/*$
 * local:   struct _IO_FILE* addr = new File;
 * ensures: stdout == addr;
 * ensures: stdout->_fileno == 1;
 */
struct _IO_FILE *stdout;

/*$
 * local:   struct _IO_FILE* addr = new File;
 * ensures: stderr == addr;
 * ensures: stderr->_fileno == 2;
 */
struct _IO_FILE *stderr;

/*$
 * local: FILE* f = new File;
 * local: int fd = new FileDescriptor;
 * ensures: f->_fileno == fd;
 * ensures: return == f;
 */
FILE* _alloc_FILE();

const char *const sys_errlist[128]; // TODO: actual size

/*$
 * requires: valid_string(__filename);
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
int remove (const char *__filename);

/*$
 * requires: valid_string(__old);
 * requires: valid_string(__new);
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
int rename (const char *__old, const char *__new);


#ifdef __USE_ATFILE

/*$
 * requires: valid_string(__old);
 * requires: valid_string(__new);
 * requires: __oldfd in FileDescriptor;
 * requires: __newfd in FileDescriptor;
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
int renameat (int __oldfd, const char *__old, int __newfd, const char *__new);

#endif

/*$
 * case "success" {
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *tmpfile (void);

static char _tmpnam_buf[L_tmpnam];

/*$
 * warn: "avoid using function tmpnam";
 *
 * requires: __s != NULL implies size(__s) >= L_tmpnam;
 *
 * case "use_s" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_tmpnam - 1];
 *   ensures: valid_primed_substring(__s, L_tmpnam);
 *   ensures: return == __s;
 * }
 * 
 * case "use_static" {
 *   assumes: __s == NULL;
 *   assigns: _tmpnam_buf[0, size(_tmpnam_buf) - 1];
 *   ensures: valid_primed_string(_tmpnam_buf);
 *   ensures: return == _tmpnam_buf;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *tmpnam (char *__s);

#ifdef __USE_MISC

/*$
 * warn: "avoid using function tmpnam_r";
 *
 * requires: size(__s) >= L_tmpnam;
 *
 * case "success" {
 *   assigns: __s[0, L_tmpnam - 1];
 *   ensures: valid_primed_substring(__s, L_tmpnam);
 *   ensures: return == __s;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *tmpnam_r (char *__s);

#endif


#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * warn: "tempnam is obsolete, do not call";
 */
char *tempnam (const char *__dir, const char *__pfx);

#endif

/*$
 * requires: __stream in File;
 * requires: __stream->_fileno in FileDescriptor;
 *
 * case "success" {
 *   local:   void* addr = _mopsa_int_to_fd(__stream->_fileno);
 *   ensures: return == 0;
 *   free:    __stream;
 *   free:    addr;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int fclose (FILE *__stream);

/*$
 * requires: __stream != NULL implies (
 *             __stream in File and
 *             __stream->_fileno in FileDescriptor
 *           );
 *
 * case "success" {
 *   assumes:  __stream != NULL;
 *   local:   void* addr = _mopsa_int_to_fd(__stream->_fileno);
 *   ensures: return == 0;
 *   free:    __stream;
 *   free:    addr;
 * }
 *
 * case "flushall" {
 *   assumes: __stream == NULL;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
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
 * warn: "fopen: characters in argument __modes are not validated";
 *
 * requires: valid_string(__filename);
 * requires: valid_string(__modes);
 *
 * case "success" {
 *   local:   FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *fopen (const char *__restrict __filename,
             const char *__restrict __modes);

/*$
 * warn: "freopen: characters in argument __modes are not validated";
 *
 * requires: valid_string(__filename);
 * requires: valid_string(__modes);
 * requires: __stream in File;
 *
 * case "success" {
 *   local:   int fd = new FileDescriptor;
 *   assigns: __stream->_fileno;
 *   ensures: (__stream->_fileno)' == fd;
 *   ensures: return == __stream;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *freopen (const char *__restrict __filename,
               const char *__restrict __modes,
               FILE *__restrict __stream);


#ifdef __USE_POSIX

/*$
 * requires: valid_string(__modes);
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   local:   FILE* f = new File;
 *   ensures: f->_fileno == __fd;
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *fdopen (int __fd, const char *__modes);

#endif


// TODO: omitted __USE_GNU fopencookie

#ifdef __USE_XOPEN2K8

/*$
 * // TODO: if __s != NULL, the buffer pointed to by __s becomes volatile
 *
 * requires: valid_string(__modes);
 * requires: __s != NULL implies size(__s) >= __len;
 *
 * case "success" {
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *fmemopen (void *__s, size_t __len, const char *__modes);

/*$
 * // TODO: *__bufloc and __sizeloc become volatile
 *
 * case "success" {
 *   local: FILE* r = _alloc_FILE();
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *open_memstream (char **__bufloc, size_t *__sizeloc);

#endif

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != NULL implies size(__buf) >= BUFSIZ;
 */
void setbuf (FILE *__restrict __stream, char *__restrict __buf);

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != NULL implies size(__buf) >= __n;
 * requires: __modes in [0, 2];
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return != 0;
 * }
 */
int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
             int __modes, size_t __n);

#ifdef	__USE_MISC

/*$
 * // TODO: __buf becomes volatile
 *
 * requires: __stream in File;
 * requires: __buf != NULL implies size(__buf) >= __size;
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
 * requires: valid_string(__format);
 */
int fprintf (FILE *__restrict __stream,
             const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments, check size of __s
 *
 * requires: valid_string(__format);
 */
int sprintf (char *__restrict __s,
             const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __s in File;
 * requires: valid_string(__format);
 */
int vfprintf (FILE *__restrict __s, const char *__restrict __format,
              _G_va_list __arg);


/*$
 * // TODO: internal glibc function, undocumented
 * requires: __s in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 */
//int _IO_vfprintf (_IO_FILE *__restrict __s, const char *__restrict __format,
//                  _IO_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__format);
 */
int vprintf (const char *__restrict __format, _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments, check size of __s
 *
 * requires: valid_string(__format);
 */
int vsprintf (char *__restrict __s, const char *__restrict __format,
              _G_va_list __arg);

#if defined __USE_ISOC99 || defined __USE_UNIX98

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: size(__s) >= __maxlen;
 * requires: valid_string(__format);
 */
int snprintf (char *__restrict __s, size_t __maxlen,
              const char *__restrict __format, ...);;

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: size(__s) >= __maxlen;
 * requires: valid_string(__format);
 */
int vsnprintf (char *__restrict __s, size_t __maxlen,
               const char *__restrict __format, _G_va_list __arg);

#endif

#ifdef __USE_GNU

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__f);
 *
 * case "success" {
 *   local:    char* r = new Memory;
 *   assigns:  *__ptr;
 *   ensures:  size(r) > 0;
 *   ensures:  valid_string(r);
 *   ensures:  (*__ptr)' == r;
 *   ensures:  return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: *__ptr;
 *   ensures: return == -1;
 * }
 */
int vasprintf (char **__restrict __ptr, const char *__restrict __f,
               _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__fmt);
 *
 * case "success" {
 *   local:    char* r = new Memory;
 *   assigns:  *__ptr;
 *   ensures:  size(r) > 0;
 *   ensures:  valid_string(r);
 *   ensures:  (*__ptr)' == r;
 *   ensures:  return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: *__ptr;
 *   ensures: return == -1;
 * }
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
 * requires: valid_string(__fmt);
 */
int dprintf (int __fd, const char *__restrict __fmt, ...);

#endif

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __stream in File;
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int fscanf (FILE *__restrict __stream,
            const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int scanf (const char *__restrict __format, ...);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__s);
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int sscanf (const char *__restrict __s,
            const char *__restrict __format, ...);


#ifdef	__USE_ISOC99

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: __s in File;
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int vfscanf (FILE *__restrict __s, const char *__restrict __format,
             _G_va_list __arg);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __s in File;
 * requires: exists int i in [0, size(__format) - 1]: __format[i] == 0;
 * assigns:  _errno;
 */
//int _IO_vfscanf (_IO_FILE *__restrict __s, const char *__restrict __format,
//                 _IO_va_list __arg, int* arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int vscanf (const char *__restrict __format, _G_va_list __arg);

/*$
 * // TODO: check format, check variable arguments
 *
 * requires: valid_string(__s);
 * requires: valid_string(__format);
 * assigns:  _errno;
 */
int vsscanf (const char *__restrict __s,
             const char *__restrict __format, _G_va_list __arg);

#endif /* Use ISO C9x.  */


/*$
 * requires: __stream in File;
 * ensures: return in [0, 255] or return == EOF;
 */
int fgetc (FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 * ensures: return in [0, 255] or return == EOF;
 */
int _IO_getc (_IO_FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 * ensures: return in [0, 255] or return == EOF;
 */
int _IO_peekc_locked (_IO_FILE *__stream);

/*$
 * ensures: return in [0, 255] or return == EOF;
 */
int getchar (void);


#ifdef __USE_POSIX199506

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: return in [0, 255] or return == EOF;
 */
int getc_unlocked (FILE *__stream);

/*$
 * // TODO: not thread-safe
 * ensures: return in [0, 255] or return == EOF;
 */
int getchar_unlocked (void);

#endif /* Use POSIX.  */



#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: return in [0, 255] or return == EOF;
 */
int fgetc_unlocked (FILE *__stream);

#endif /* Use MISC.  */

/*$
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int fputc (int __c, FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int _IO_putc (int __c, _IO_FILE *__stream);

/*$
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int putchar (int __c);


#ifdef __USE_MISC

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int fputc_unlocked (int __c, FILE *__stream);

#endif /* Use MISC.  */


#ifdef __USE_POSIX199506

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int putc_unlocked (int __c, FILE *__stream);

/*$
 * // TODO: not thread-safe
 * ensures: (return == (unsigned char) __c) or (return == EOF);
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
 * ensures:  (return == 0) or (return == EOF);
 */
int putw (int __w, FILE *__stream);

#endif

/*$
 * requires: __stream in File;
 * requires: size(__s) >= __n;
 * assigns:  __s[0, size(__s) - 1];
 * ensures:  valid_primed_string(__s);
 * ensures:  (return == __s) or (return == NULL);
 */
char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);

/*$
 * warn: "gets: dangerous function, do not use";
 * ensures: (return == __s) or (return == NULL);
 */
char *gets (char *__s);


#ifdef __USE_GNU

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: size(__s) >= __n;
 * assigns:  __s[0, size(__s) - 1];
 * ensures:  valid_primed_string(__s);
 * ensures:  (return == __s) or (return == NULL);
 */
char *fgets_unlocked (char *__restrict __s, int __n,
                      FILE *__restrict __stream);

#endif


#ifdef __USE_XOPEN2K8

/*$
 * requires: __stream in File;
 * requires: *__lineptr != NULL implies size(*__lineptr) >= *__n;
 * 
 * case "realloc" {
 *   assumes:  *__lineptr != NULL;
 *   requires: *__lineptr in Memory;
 *   local:    char* r = new Memory;
 *   assigns:  *__lineptr;
 *   assigns:  *__n;
 *   free:     *__lineptr;
 *   ensures:  size(r) == (*__n)';
 *   ensures:  (*__lineptr)' == r;
 *   ensures:  return >= 0;
 * }
 *
 * case "alloc" {
 *   assumes: *__lineptr == NULL;
 *   local:   char* r = new Memory;
 *   assigns: *__lineptr;
 *   assigns: *__n;
 *   ensures: size(r) == (*__n)';
 *   ensures: (*__lineptr)' == r;
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t getdelim (char **__restrict __lineptr,
                  size_t *__restrict __n, int __delimiter,
                  FILE *__restrict __stream);

/*$
 * local:   size_t r  = getdelim(__lineptr, __n, '\n', __stream);
 * ensures: return == r;
 */
ssize_t getline (char **__restrict __lineptr,
                 size_t *__restrict __n,
                 FILE *__restrict __stream);

#endif

/*$
 * requires: __stream in File;
 * requires: valid_string(__s);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int fputs (const char *__restrict __s, FILE *__restrict __stream);

/*$
 * requires: valid_string(__s);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
 */
int puts (const char *__s);

/*$
 * requires: __stream in File;
 * ensures: (return == (unsigned char) __c) or (return == EOF);
 */
int ungetc (int __c, FILE *__stream);

/*$
 * requires: __stream in File;
 * requires: size(__ptr) >= __size * __n;
 * assigns:  __ptr[0, __size * __n - 1];
 * ensures:  return in [0, __n];
 */
size_t fread (void *__restrict __ptr, size_t __size,
              size_t __n, FILE *__restrict __stream);

/*$
 * requires: __s in File;
 * requires: size(__ptr) >= __size * __n;
 * ensures:  return in [0, __n];
 */
size_t fwrite (const void *__restrict __ptr, size_t __size,
               size_t __n, FILE *__restrict __s);

#ifdef __USE_GNU

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: valid_string(__s);
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == EOF;
 * }
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
 * ensures:  return in [0, __n];
 */
size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                       size_t __n, FILE *__restrict __stream);

/*$
 * // TODO: not thread-safe
 * requires: __stream in File;
 * requires: size(__ptr) >= __size * __n;
 * ensures:  return in [0, __n];
 */
size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
                        size_t __n, FILE *__restrict __stream);

#endif

/*$
 * requires: __stream in File;
 * requires: __whence in [0, 2];
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
int fseek (FILE *__stream, long int __off, int __whence);

/*$
 * requires: __stream in File;
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
long int ftell (FILE *__stream);

/*$
 * requires: __stream in File;
 */
void rewind (FILE *__stream);

#if defined __USE_LARGEFILE || defined __USE_XOPEN2K

/*$
 * requires: __stream in File;
 * requires: __whence in [0, 2];
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
int fseeko (FILE *__stream, __off_t __off, int __whence);

/*$
 * requires: __stream in File;
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__off_t ftello (FILE *__stream);

#endif

/*$
 * requires: __stream in File;
 *
 * case "success" {
 *   assigns: *__pos;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);

/*$
 * requires: __stream in File;
 * // TODO requires: size(__pos) >= sizeof(fpos_t);
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
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 */
int _IO_feof (_IO_FILE *__stream);

/*$
 * requires: __stream in File;
 */
int ferror (FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 */
int _IO_ferror (_IO_FILE *__stream);

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
 * requires: valid_string(__s);
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
 * requires: valid_string(__command);
 * requires: valid_string(__modes);
 *
 * case "success" {
 *   local:   FILE* f = _alloc_FILE();
 *   ensures: return == f;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
FILE *popen (const char *__command, const char *__modes);

/*$
 * requires: __stream in File;
 * assigns:  _errno;
 * free:     __stream;
 */
int pclose (FILE *__stream);

#endif


#ifdef	__USE_POSIX

static char _ctermid_buf[L_ctermid];

/*$
 * requires: __s != NULL implies size(__s) >= L_ctermid;
 *
 * case "buf" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_ctermid - 1];
 *   ensures: valid_primed_substring(__s, L_ctermid);
 *   ensures: return == __s;
 * }
 *
 * case "nobuf" {
 *   assumes: __s == NULL;
 *   assigns: _ctermid_buf[0, L_ctermid - 1];
 *   ensures: valid_primed_substring(_ctermid_buf, L_ctermid);
 *   ensures: return == &_ctermid_buf[0];
 * }
 */
char *ctermid (char *__s);

#endif /* Use POSIX.  */


#if (defined __USE_XOPEN && !defined __USE_XOPEN2K) || defined __USE_GNU

static char _cuserid_buf[L_cuserid];

/*$
 * requires: __s != NULL implies size(__s) >= L_cuserid;
 *
 * case "buf" {
 *   assumes: __s != NULL;
 *   assigns: __s[0, L_cuserid - 1];
 *   ensures: valid_primed_substrint(__s, L_cuserid);
 *   ensures: return == __s;
 * }
 *
 * case "nobuf" {
 *   assumes: __s == NULL;
 *   assigns: _cuserid_buf[0, L_cuserid - 1];
 *   ensures: valid_primed_substring(_cuserid_buf, L_cuserid);
 *   ensures: return == &_cuserid_buf[0];
 * }
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
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 */
void _IO_flockfile (_IO_FILE *__stream);

/*$
 * requires: __stream in File;
 */
int ftrylockfile (FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 */
int _IO_ftrylockfile (_IO_FILE *__stream);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __stream in File;
 */
void _IO_funlockfile (_IO_FILE *__stream);

/*$
 * requires: __stream in File;
 */
void funlockfile (FILE *__stream);

#endif /* POSIX */


// glibc internals


/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
int __underflow (_IO_FILE *__fd);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
int __uflow (_IO_FILE *__fd);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
int __overflow (_IO_FILE *__fd, int arg1);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
__ssize_t _IO_padn (_IO_FILE *__fd, int, __ssize_t __len);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 * requires: size(__ptr) >= __len;
 * assigns:  __ptr[0, __len - 1];
 * ensures:  return <= __len;
 */
size_t _IO_sgetn (_IO_FILE *__fd, void *__ptr, size_t __len);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
__off64_t _IO_seekoff (_IO_FILE *__fd, __off64_t __off, int arg1, int arg2);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
__off64_t _IO_seekpos (_IO_FILE *__fd, __off64_t __off, int arg1);

/*$
 * // TODO: internal glibc function, undocumented
 * requires: __fd in File;
 */
void _IO_free_backup_area (_IO_FILE *__fd);
