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

#define __USE_GNU

/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <string.h>
#include "mopsa_libc_utils.h"

/*$$
 * predicate no_overlap(s1, n1, s2, n2):
 *   base(s1) != base(s2) or
 *   not ((unsigned char*)s1 <= (unsigned char*)s2 + n2) or
 *   not ((unsigned char*)s2 <= (unsigned char*)s1 + n1);
 */

/*$
 * requires: no_overlap(__src, __len, __dest, __len);
 *
 * case "copy" {
 *   assumes: __len >= 1;
 *   requires: valid_bytes(__src, __len);
 *   requires: valid_bytes(__dest, __len);
 *   assigns: ((unsigned char*)__dest)[0, (int)__len - 1];
 *   ensures: forall int i in [0, (int)__len - 1]: (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 * }
 *
 * case "nop" {
 *   assumes: __len == 0;
 * }
 *
 * ensures: return == __dest;
 */
void *memcpy (void *__restrict __dest, const void *__restrict __src,
              size_t __len);

/*$
 * requires: valid_bytes(__src, __len);
 * requires: valid_bytes(__dest, __len);
 * assigns: ((unsigned char*)__dest)[0, (int)__len - 1];
 * ensures: forall int i in [0, (int)__len - 1]: (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 */
void *memmove (void *__dest, const void *__src, size_t __len);


/*$
 * requires: no_overlap(__src, __n, __dest, __n);
 * requires: valid_bytes(__src, __n);
 * requires: valid_bytes(__dest, __n);
 * assigns: ((unsigned char*)__dest)[0, (int)__n - 1];
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)__n - 1]: ((unsigned char*)__src)[i] != __c;
 *   ensures: forall int i in [0, (int)__n - 1]: (((unsigned char*)__dest)[i])' == ((unsigned char*)__src)[i];
 *   ensures: return == NULL;
 * }
 *
 * case "found" {
 *   assumes: exists int i in [0, (int)__n - 1]: ((unsigned char*)__src)[i] == __c;
 *   ensures: exists int i in [0, (int)__n - 1]: (
 *              (unsigned char*)__src[i] == __c and
 *              ((unsigned char*)__dest[i])' == __c and
 *              return == (unsigned char*)__src + i + 1 and
 *              forall int j in [0, i - 1]: (
 *                (unsigned char*)__src[j] != __c and
 *                ((unsigned char*)__dest[j])' == ((unsigned char*)__src)[j]
 *              ) and
 *              forall int j in [i, (int)__n - 1]: (((unsigned char*)__dest)[j])' == ((unsigned char*)__dest)[j]
 *            );
 * }
 */
void *memccpy (void *__restrict __dest, const void *__restrict __src,
		      int __c, size_t __n);

/*$
 * requires: valid_bytes(__dest, __len);
 * assigns: ((unsigned char*)__dest)[0, (int)__len - 1];
 * ensures: forall int i in [0, (int)__len - 1]: (((unsigned char*)__dest)[i])' == __ch;
 * ensures: return == __dest;
 */
void *memset (void *__dest, int __ch, size_t __len);


/*$
 * requires: valid_bytes(__s1, __n);
 * requires: valid_bytes(__s2, __n);
 *
 * case "equal" {
 *   assumes: forall int i in [0, (int)__n - 1]: ((unsigned char*)__s1)[i] == ((unsigned char*)__s2)[i];
 *   ensures: return == 0;
 * }
 *
 * case "notequal" {
 *   assumes: exists int i in [0, (int)__n - 1]: ((unsigned char*)__s1)[i] != ((unsigned char*)__s2)[i];
 *   ensures: exists int i in [0, (int)__n - 1]: (
 *             (unsigned char*)__s1[i] != (unsigned char*)__s2[i] and
 *             forall int j in [0, i - 1]: ((unsigned char*)__s1)[j] == ((unsigned char*)__s2)[j] and
 *             (((unsigned char*)__s1)[i] - ((unsigned char*)__s2)[i] > 0 implies return > 0) and
 *             (((unsigned char*)__s1)[i] - ((unsigned char*)__s2)[i] < 0 implies return < 0)
 *            );
 * }
 */
int memcmp (const void *__s1, const void *__s2, size_t __n);

/*$
 * requires: valid_bytes(__s, __n);
 *
 * case "found" {
 *   assumes: exists int i in [0, (int)__n - 1]: ((unsigned char*)__s)[i] == __c;
 *   ensures: exists int i in [0, (int)__n - 1]: (
 *              (unsigned char*)__s[i] == __c and
 *              forall int j in [0, i - 1]: ((unsigned char*)__s)[i] != __c and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)__n - 1]: ((unsigned char*)__s)[i] != __c;
 *   ensures: return == NULL;
 * }
 */
void *memchr (const void *__s, int __c, size_t __n);


/*$
 * requires: valid_ptr(__s);
 * requires: exists int i in [0, bytes(__s) - offset(__s) - 1]: ((unsigned char*)__s)[i] == __c;
 * ensures:  exists int i in [0, bytes(__s) - offset(__s) - 1]: (
 *             (unsigned char*)__s[i] == __c and
 *             forall int j in [0, i - 1]: ((unsigned char*)__s)[i] != __c and
 *             return == (unsigned char*)__s + i
 *           );
 */
void *rawmemchr (const void *__s, int __c);


/*$
 * alias: rawmemchr;
 */
void *__rawmemchr (const void *__s, int __c);


/*$
 * requires: valid_bytes(__s, __n);
 *
 * case "found" {
 *   assumes: exists int i in [0, (int)__n - 1]: ((unsigned char*)__s)[i] == __c;
 *   ensures: exists int i in [0, (int)__n - 1]: (
 *              (unsigned char*)__s[i] == __c and
 *              forall int j in [0, i - 1]: ((unsigned char*)__s)[i] != __c and
 *              return == (unsigned char*)__s + i
 *            );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)__n - 1]: ((unsigned char*)__s)[i] != __c;
 *   ensures: return == NULL;
 * }
 */
void *memrchr (const void *__s, int __c, size_t __n);



/*$
 * requires: valid_string(__s);
 * ensures:  exists int i in [0, size(__s) - offset(__s) - 1]: ( 
 *             __s[i] == 0 and 
 *             (forall int j in [0, i - 1]: __s[j] != 0) and
 *             return == i 
 *          );
 */
size_t strlen (const char *__s);

/*$
 * alias: strlen;
 */
size_t __builtin_strlen (const char *__s);


/*$
 * requires: valid_ptr(__string);
 *
 * case "string-smaller-than-maxlen" {
 *   assumes: size(__string) - offset(__string) <= __maxlen;
 *   requires: valid_string(__string);
 *   local: size_t len = strlen(__string);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen-with-zero" {
 *   assumes: size(__string) - offset(__string) >= __maxlen + 1;
 *   assumes: exists int i in [0, __maxlen]: __string[i] == 0;
 *   local: size_t len = strlen(__string);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen-without-zero" {
 *   assumes: size(__string) - offset(__string) >= __maxlen + 1;
 *   assumes: forall int i in [0, __maxlen]: __string[i] != 0;
 *   ensures: return == __maxlen;
 * }
 */
size_t strnlen (const char *__string, size_t __maxlen);


/*$
 * requires: valid_string(__src);
 * local: size_t src_len = strlen(__src);
 * requires: no_overlap(__src, src_len, __dest, src_len);
 * requires: valid_bytes(__dest, src_len);
 * assigns: __dest[0, src_len];
 * ensures: forall int i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest;
 */
char *strcpy (char *__restrict __dest, const char *__restrict __src);


/*$
 * local: size_t src_nlen = strnlen(__src, __len);
 * requires: no_overlap(__src, src_nlen, __dest, src_nlen);
 * requires: valid_bytes(__dest, __len);
 * assigns: __dest[0, (int)__len - 1];
 * ensures: forall int i in [0, (int)src_nlen - 1]: (__dest[i])' == __src[i];
 * ensures: forall int i in [src_nlen, (int)__len - 1]: (__dest[i])' == 0;
 * ensures: return == __dest;
 */
char *strncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __len);

/*$
 * requires: valid_string(__src);
 * requires: valid_string(__dest);
 * local: size_t src_len = strlen(__src);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, src_len, __dest, dest_len + src_len);
 * requires: valid_bytes(__dest, dest_len + src_len);
 * assigns: __dest[0, dest_len + src_len];
 * ensures: forall int i in [0, (int)dest_len - 1]: (__dest[i])' == __dest[i];
 * ensures: forall int i in [dest_len, dest_len + src_len]: (__dest[i])' == __src[i - dest_len];
 * ensures: return == __dest;
 */
char *strcat (char *__restrict __dest, const char *__restrict __src);


/*$
 * requires: valid_string(__dest);
 * local: size_t src_nlen = strnlen(__src, __len);
 * local: size_t dest_len = strlen(__dest);
 * requires: no_overlap(__src, src_nlen, __dest, dest_len + src_nlen);
 * requires: valid_bytes(__dest, dest_len + src_nlen);
 * assigns: __dest[0, dest_len + src_nlen];
 * ensures: forall int i in [0, (int)dest_len - 1]: (__dest[i])' == __dest[i];
 * ensures: forall int i in [dest_len, dest_len + src_nlen]: (__dest[i])' == __src[i - dest_len];
 * ensures: return == __dest;
 */
char *strncat (char *__restrict __dest, const char *__restrict __src,
               size_t __len);

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 */
int strcmp (const char *__s1, const char *__s2);

/*$
 * alias: strcmp;
 */
int __builtin_strcmp (const char *__s1, const char *__s2);


/*$
 * requires: valid_ptr(__s1);
 * requires: valid_ptr(__s2);
 * requires: size(__s1) < offset(__s1) + __n implies valid_string(__s1);
 * requires: size(__s2) < offset(__s2) + __n implies valid_string(__s2);
 */
int strncmp (const char *__s1, const char *__s2, size_t __n);

/*$
 * alias: strncmp;
 */
int __builtin_strncmp (const char *__s1, const char *__s2, size_t __n);


/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 * assigns:  _errno;
 */
int strcoll (const char *__s1, const char *__s2);

/*$
 * requires: valid_string(__src);
 * requires: valid_bytes(__dest, __n);
 * requires: no_overlap(__src, __n, __dest, __n);
 * assigns: __dest[0, (int)__n - 1];
 * assigns: _errno;
 * ensures: return < __n implies (__dest[return])' == 0;
 */
size_t strxfrm (char *__restrict __dest,
                const char *__restrict __src, size_t __n);

#ifdef __USE_XOPEN2K8


// type __locale_t has been renamed local_t starting from glibc 2.26
#if __GLIBC_MINOR__ < 26

/*$
 * requires: valid_string(__s1);
 * requires: valid_string(__s2);
 * assigns:  _errno;
 */
int strcoll_l (const char *__s1, const char *__s2, __locale_t __l);

/*$
 * requires: valid_string(__src);
 * requires: valid_bytes(__dest, __n);
 * requires: no_overlap(__src, __n, __dest, __n);
 * assigns: __dest[0, (int)__n - 1];
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
 * requires: valid_bytes(__dest, __n);
 * requires: no_overlap(__src, __n, __dest, __n);
 * assigns: __dest[0, (int)__n - 1];
 * assigns: _errno;
 * ensures: return < __n implies (__dest[return])' == 0;
 */
size_t strxfrm_l (char *__dest, const char *__src, size_t __n, locale_t __l);

#endif

#endif


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

/*$
 * alias: strdup;
 */
char *__strdup (const char *__s);


#if defined __USE_XOPEN2K8

/*$
 * requires: valid_bytes(__s, __n) or valid_string(__s);
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
 * requires: valid_string(__s);
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists int i in [0, (int)len - 1]: (
 *             __s[i] == __c and
 *             (forall int j in [0, i - 1]: __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)len - 1]: __s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strchr (const char *__s, int __c);

/*$
 * alias: strchr;
 */
char *__builtin_strchr (const char *__s, int __c);


/*$
 * requires: valid_string(__s);
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists int i in [0, (int)len - 1]: (
 *             __s[i] == __c and
 *             (forall int j in [i + 1, (int)len - 1]: __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)len - 1]: __s[i] != __c;
 *   ensures: return == NULL;
 * }
 */
char *strrchr (const char *__s, int __c);


#ifdef __USE_GNU

/*$
 * requires: valid_string(__s);
 * local: size_t len = strlen(__s);
 *
 * case "found" {
 *   ensures:  exists int i in [0, (int)len - 1]: (
 *             __s[i] == __c and
 *             (forall int j in [0, i - 1]: __s[j] != __c) and
 *             return == __s + i
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, (int)len - 1]: __s[i] != __c;
 *   ensures: return == __s + len;
 * }
 */
char *strchrnul (const char *__s, int __c);

#endif

/*$
 * requires: valid_string(__s);
 * requires: valid_string(__reject);
 * local: size_t len = strlen(__s);
 * ensures: return in [0, len];
 */
size_t strcspn (const char *__s, const char *__reject);

/*$
 * requires: valid_string(__s);
 * requires: valid_string(__accept);
 * local: size_t len = strlen(__s);
 * ensures: return in [0, len];
 */
size_t strspn (const char *__s, const char *__accept);

/*$
 * requires: valid_string(__s);
 * requires: valid_string(__accept);
 * local: size_t len = strlen(__s);
 * ensures: return == NULL or (exists int i in [0, (int)len - 1]: return == __s + i);
 */
char *strpbrk (const char *__s, const char *__accept);

/*$
 * requires: valid_string(__haystack);
 * requires: valid_string(__needle);
 * local: size_t len1 = strlen(__haystack);
 * local: size_t len2 = strlen(__needle);
 * ensures: return == NULL or (exists int i in [0, (int)len1 - (int)len2]: return == __haystack + i);
 */
char *strstr (const char *__haystack, const char *__needle);


static char* _strtok_buf = NULL;
static size_t _strtok_len = 0;

/*$
 * requires: valid_string(__delim);
 *
 * case "first" {
 *   assumes: __s != NULL;
 *   requires: valid_string(__s);
 *   local: size_t len = strlen(__s);
 *   assigns: __s[0, (int)len - 1];
 *   assigns: _strtok_buf;
 *   assigns: _strtok_len;
 *   ensures: exists int i in [0, (int)len - 1]: return == __s + i;
 *   ensures: _strtok_buf' == __s;
 *   ensures: _strtok_len' == len;
 * }
 *
 * case "next" {
 *   assumes: __s == NULL;
 *   assigns: _strtok_buf[0, (int)_strtok_len - 1];
 *   ensures: exists int i in [0, (int)_strtok_len - 1]: return == _strtok_buf + i;
 *  }
 *
 * case "last" {
 *   assumes: __s == NULL;
 *  ensures: return == NULL;
 *  }
 */
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


char *strerror (int __errnum);

#if defined __USE_XOPEN2K && !defined __USE_GNU

int strerror_r (int __errnum, char *__buf, size_t __buflen);

#else

char *strerror_r (int __errnum, char *__buf, size_t __buflen);

#endif

#ifdef __USE_XOPEN2K8

#if __GLIBC_MINOR__ < 26

char *strerror_l (int __errnum, __locale_t __l);

#else

char *strerror_l (int __errnum, locale_t __l);

#endif

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
