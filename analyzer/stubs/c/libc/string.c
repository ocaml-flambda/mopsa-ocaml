/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/

#define __USE_GNU

/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <string.h>
#include "mopsa_libc_utils.h"

/*$$
 * predicate no_overlap(s1, s2, n):
 *   base(s1) != base(s2) or
 *   not ((unsigned char*)s1 <= (unsigned char*)s2 + n) or
 *   not ((unsigned char*)s2 <= (unsigned char*)s1 + n);
 */

/*$
 * requires: no_overlap(__src, __dest, __n);
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 * assigns: ((unsigned char*)__dest)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: ((unsigned char*)__dest[i])' == (unsigned char*)__src[i];
 */
void *memcpy (void *__restrict __dest, const void *__restrict __src,
              size_t __n);

/*$
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 * assigns: ((unsigned char*)__dest)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: ((unsigned char*)__dest[i])' == (unsigned char*)__src[i];
 */
void *memmove (void *__dest, const void *__src, size_t __n);


#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * requires: no_overlap(__src, __dest, __n);
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 *
 * case "notfound" {
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__src[i] != __c;
 *   assigns: ((unsigned char*)__dest)[0, __n - 1];
 *   ensures: forall int i in [0, __n - 1]: ((unsigned char*)__dest[i])' == (unsigned char*)__src[i];
 *   ensures: return == NULL;
 * }
 *
 * case "found" {
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__src[i] == __c;
 *   assigns: ((unsigned char*)__dest)[0, __n - 1];
 *   ensures: exists int i in [0, __n - 1]: (
 *              (unsigned char*)__src[i] == __c and
 *              ((unsigned char*)__dest[i])' == __c and
 *              return == (unsigned char*)__src + i + 1 and
 *              forall int j in [0, i - 1]: (
 *                (unsigned char*)__src[j] != __c and
 *                ((unsigned char*)__dest[j])' == (unsigned char*)__src[j]
 *              ) and
 *              forall int j in [i, __n - 1]: ((unsigned char*)__dest[j])' == (unsigned char*)__dest[j]
 *            );
 * }
 */
void *memccpy (void *__restrict __dest, const void *__restrict __src,
		      int __c, size_t __n);

#endif /* Misc || X/Open.  */

/*$
 * requires: size(__s) >= __n;
 * assigns: ((unsigned char*)__s)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: ((unsigned char*)__s[i])' == __c;
 * ensures: return == __s;
 */
void *memset (void *__s, int __c, size_t __n);

/*$
 * requires: size(__s1) >= __n;
 * requires: size(__s2) >= __n;
 *
 * case "equal" {
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s1[i] == (unsigned char*)__s2[i];
 *   ensures: return == 0;
 * }
 *
 * case "notequal" {
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s1[i] != (unsigned char*)__s2[i];
 *   ensures: exists int i in [0, __n - 1]: (
 *             (unsigned char*)__s1[i] != (unsigned char*)__s2[i] and
 *             forall int j in [0, i - 1]: (unsigned char*)__s1[j] == (unsigned char*)__s2[j] and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] > 0 implies return > 0) and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] < 0 implies return < 0)
 *            );
 * }
 */
int memcmp (const void *__s1, const void *__s2, size_t __n);

/*$
 * requires: size(__s) >= __n;
 *
 * case "found" {
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s[i] == __c;
 *   ensures: exists int i in [0, __n - 1]: (
 *              (unsigned char*)__s[i] == __c and
 *              forall int j in [0, i - 1]: (unsigned char*)__s[i] != __c and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
void *memchr (const void *__s, int __c, size_t __n);

#ifdef __USE_GNU

/*$
 * requires: size(__s) >= __n;
 *
 * case "found" {
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s[i] == __c;
 *   ensures: exists int i in [0, __n - 1]: (
 *              (unsigned char*)__s[i] == __c and
 *              forall int j in [0, i - 1]: (unsigned char*)__s[i] != __c and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
void *memrchr (const void *__s, int __c, size_t __n);

#endif

/*$
 * local: size_t src_len = strlen(__src);
 * requires: no_overlap(__src, __dest, src_len);
 * requires: size(__dest) >= src_len;
 * assigns: __dest[0, src_len - 1];
 * ensures: forall int i in [0, src_len - 1]: (__dest[i])' == __src[i];
 * ensures: return == __dest;
 */
char *strcpy (char *__restrict __dest, const char *__restrict __src);

/*$
 * local: size_t src_len = strlen(__src);
 * requires: no_overlap(__src, __dest, src_len);
 * requires: size(__dest) >= src_len or 
 *           size(__dest) >= __n;
 *
 * case "zero" {
 *   assumes: __n > src_len + 1;
 *   assigns: __dest[0, src_len];
 *   ensures: forall int i in [0, src_len]: (__dest[i])' == __src[i];
 *   ensures: return == __dest;
 * }
 * 
 * case "nozero" {
 *   assumes: __n <= src_len + 1;
 *   assigns: __dest[0, __n];
 *   ensures: forall int i in [0, __n - 1]: (__dest[i])' == __src[i];
 *   ensures: return == __dest;
 * }
 */
char *strncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __n);

/*$
 * local: size_t src_len = strlen(__src);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, __dest, src_len);
 * requires: size(__dest) >= src_len + dest_len + 1;
 * assigns: __dest[0, src_len + dest_len + 1];
 * ensures: forall int i in [0, dest_len - 1]: (__dest[i])' == __dest[i];
 * ensures: forall int i in [0, src_len]: (__dest[i + dest_len])' == __src[i];
 * ensures: return == __dest;
 */
char *strcat (char *__restrict __dest, const char *__restrict __src);

/*$
 * local: size_t src_len = strlen(__src);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, __dest, src_len);
 * requires: size(__dest) >= src_len + dest_len + 1 or
 *           size(__dest) >= __n + dest_len + 1;
 * 
 * case "zero-found" {
 *   assumes: __n > src_len + 1;
 *   assigns: __dest[0, src_len + dest_len + 1];
 *   ensures: forall int i in [0, dest_len - 1]: (__dest[i])' == __dest[i];
 *   ensures: forall int i in [0, src_len]: (__dest[i + dest_len])' == __src[i];
 *   ensures: return == __dest;
 * }
 *
 * case "zero-not-found" {
 *   assumes: __n <= src_len + 1;
 *   assigns: __dest[0, __n + dest_len + 1];
 *   ensures: forall int i in [0, dest_len - 1]: (__dest[i])' == __dest[i];
 *   ensures: forall int i in [0, __n - 1]: (__dest[i + dest_len])' == __src[i];
 *   ensures: (__dest[dest_len + __n])' == 0;
 *   ensures: return == __dest;
 * }
 */
char *strncat (char *__restrict __dest, const char *__restrict __src,
               size_t __n);

/*$
 * local: size_t len1 = strlen(__s1);
 * local: size_t len2 = strlen(__s2);
 *
 * case "equal" {
 *   assumes: len1 == len2 and
 *            forall int i in [0, len1 - 1]: __s1[i] == __s2[i];
 *   ensures: return == 0;
 * }
 *
 * case "not-equal" {
 *   assumes: exists int i in [0, len1 - 1]: (
 *              i <= len2 - 1 and 
 *              __s1[i] != __s2[i]
 *            );
 *   ensures: exists int i in [0, len1 - 1]: (
 *              i <= len2 - 1 and 
 *              __s1[i] != __s2[i] and
 *              forall int j in [0, i - 1]: __s1[j] == __s2[j] and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] > 0 implies return > 0) and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] < 0 implies return < 0)
 *           );
 *  }
 */
int strcmp (const char *__s1, const char *__s2);

/*$
 * requires: size(__s1) >= __n or valid_string(__s1);
 * requires: size(__s2) >= __n or valid_string(__s2);
 *
 * case "equal" {
 *   assumes: exists int l in [0, __n - 1]: (
 *              (l == __n - 1 or __s1[l] == 0) and
 *              forall int i in [0, l]: __s1[i] == __s2[i]
 *   );
 *   ensures: return == 0;
 * }
 *
 * case "notequal" {
 *   assumes: exists int l in [0, __n - 1]: (
 *              __s1[l] != __s2[l] and
 *              forall int i in [0, l - 1]: (__s1[i] != 0 or __s2[i] != 0)
 *            );
 *   ensures: exists int l in [0, __n - 1]: (
 *              __s1[l] != __s2[l] and
 *              forall int i in [0, l]: __s1[i] == __s2[i] and
 *              ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] > 0 implies return > 0) and
 *              ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] < 0 implies return < 0)
 *            );
 *   }
 */
int strncmp (const char *__s1, const char *__s2, size_t __n);

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 * assigns:  _errno;
 */
int strcoll (const char *__s1, const char *__s2);

/*$
 * requires: valid_string(__src);
 * assigns: __dest[0, __n - 1];
 * assigns: _errno;
 * ensures: return < __n implies (__dest[return])' == 0;
 */
size_t strxfrm (char *__restrict __dest,
                const char *__restrict __src, size_t __n);

#ifdef __USE_XOPEN2K8


// type __locale_t has been renamed local_t starting from glibv 2.26
#if __GLIBC_MINOR__ < 26

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 * assigns:  _errno;
 */
int strcoll_l (const char *__s1, const char *__s2, __locale_t __l);

/*$
 * requires: valid_string(__src);
 * assigns: __dest[0, __n - 1];
 * assigns: _errno;
 * ensures: return < __n implies (__dest[return])' == 0;
 */
size_t strxfrm_l (char *__dest, const char *__src, size_t __n, __locale_t __l);

#else

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 * assigns:  _errno;
 */
int strcoll_l (const char *__s1, const char *__s2, locale_t __l);

/*$
 * requires: valid_string(__src);
 * assigns: __dest[0, __n - 1];
 * assigns: _errno;
 * ensures: return < __n implies (__dest[return])' == 0;
 */
size_t strxfrm_l (char *__dest, const char *__src, size_t __n, locale_t __l);

#endif

#endif

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * local: size_t len = strlen(__s);
 *
 * case "success" {
 *   local: char* r = new Memory;
 *   ensures: return == r;
 *   ensures: size(return) == len + 1;
 *   ensures: forall int l in [0, len]: r[l] == __s[l];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *strdup (const char *__s);

#endif

#if defined __USE_XOPEN2K8

/*$
 * requires: size(__s) >= __n or valid_string(__s);
 *
 * case "success" {
 *   local: char* r = new Memory;
 *   ensures: return == r;
 *   ensures: exists int l in [0, __n]: (
 *              (l == __n or __s[l] == 0) and
 *              r[l] == 0 and
 *              size(return) == l + 1 and
 *              forall int i in [0, l - 1]: (
 *                __s[i] != 0 and
 *                r[i] == __s[i]
 *              )
 *            );
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *strndup (const char *__s, size_t __n);

#endif

/*$
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   assumes: exists int i in [0, len]: __s[i] == __c;
 *   ensures: exists int i in [0, len]: (
 *              __s[i] == __c and
 *              return == __s + i and
 *              forall int j in [0, i - 1]: __s[j] != __c
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int j in [0, len]: __s[j] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strchr (const char *__s, int __c);

/*$
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   assumes: exists int i in [0, len]: __s[i] == __c;
 *   ensures: exists int i in [0, len]: (
 *              __s[i] == __c and
 *              return == __s + i and
 *              forall int j in [i, len]: __s[j] != __c
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int j in [0, len]: __s[j] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strrchr (const char *__s, int __c);


#ifdef __USE_GNU

char *strchrnul (const char *__s, int __c);

#endif

size_t strcspn (const char *__s, const char *__reject);

size_t strspn (const char *__s, const char *__accept);

char *strpbrk (const char *__s, const char *__accept);

char *strstr (const char *__haystack, const char *__needle);

char *strtok (char *__restrict __s, const char *__restrict __delim);

char *__strtok_r (char *__restrict __s,
                  const char *__restrict __delim,
                  char **__restrict __save_ptr);

#ifdef __USE_POSIX

char *strtok_r (char *__restrict __s, const char *__restrict __delim,
                char **__restrict __save_ptr);

#endif

#ifdef __USE_GNU

char *strcasestr (const char *__haystack, const char *__needle);

#endif

#ifdef __USE_GNU

void *memmem (const void *__haystack, size_t __haystacklen,
              const void *__needle, size_t __needlelen);

void *__mempcpy (void *__restrict __dest,
                 const void *__restrict __src, size_t __n);

void *mempcpy (void *__restrict __dest,
               const void *__restrict __src, size_t __n);

#endif

size_t strlen (const char *__s);

#ifdef	__USE_XOPEN2K8

size_t strnlen (const char *__string, size_t __maxlen);

#endif

char *strerror (int __errnum);

#if defined __USE_XOPEN2K && !defined __USE_GNU

int strerror_r (int __errnum, char *__buf, size_t __buflen);

#else

char *strerror_r (int __errnum, char *__buf, size_t __buflen);

#endif

#ifdef __USE_XOPEN2K8

char *strerror_l (int __errnum, __locale_t __l);

#endif

#ifdef __USE_MISC

void explicit_bzero (void *__s, size_t __n);

char *strsep (char **__restrict __stringp,
              const char *__restrict __delim);

#endif

#ifdef	__USE_XOPEN2K8

char *strsignal (int __sig);

char *__stpcpy (char *__restrict __dest, const char *__restrict __src);

char *stpcpy (char *__restrict __dest, const char *__restrict __src);

char *__stpncpy (char *__restrict __dest,
                 const char *__restrict __src, size_t __n);

char *stpncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __n);

#endif

#ifdef	__USE_GNU

int strverscmp (const char *__s1, const char *__s2);

char *strfry (char *__string);

void *memfrob (void *__s, size_t __n);

# ifndef basename

char *basename (const char *__filename);

#endif

#endif
