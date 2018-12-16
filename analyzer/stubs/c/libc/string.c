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

/*$
 * requires: base(__dest) != base(__src) or
 *           ! ((unsigned char*)__dest <= (unsigned char*)__src + __n) or
 *           ! ((unsigned char*)__src <= (unsigned char*)__dest + __n); // no overlap
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 * assigns: ((unsigned char*)__dest)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: (unsigned char*)__dest[i] == (unsigned char*)__src[i];
 */
void *memcpy (void *__restrict __dest, const void *__restrict __src,
              size_t __n);

/*$
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 * assigns: ((unsigned char*)__dest)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: (unsigned char*)__dest[i] == old((unsigned char*)__src[i]);
 */
void *memmove (void *__dest, const void *__src, size_t __n);


#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * requires: base(__dest) != base(__src) or
 *           ! ((unsigned char*)__dest <= (unsigned char*)__src + __n) or
 *           ! ((unsigned char*)__src <= (unsigned char*)__dest + __n); // no overlap
 * requires: size(__src) >= __n;
 * requires: size(__dest) >= __n;
 *
 * case "notfound":
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__src[i] != __c;
 *   assigns: ((unsigned char*)__dest)[0, __n - 1];
 *   ensures: forall int i in [0, __n - 1]: (unsigned char*)__dest[i] == (unsigned char*)__src[i];
 *   ensures: return == _NULL;
 *
 * case "found":
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__src[i] == __c;
 *   assigns: ((unsigned char*)__dest)[0, __n - 1];
 *   ensures: exists int i in [0, __n - 1]:
 *             ((unsigned char*)__src[i] == __c and
 *              (unsigned char*)__dest[i] == __c and
 *              return == (unsigned char*)__src + i + 1 and
 *              forall int j in [0, __n - 1]: 
 *              ((j < i implies (unsigned char*)__src[j] != __c) and
 *               (j < i implies (unsigned char*)__dest[j] == (unsigned char*)__src[j]) and
 *               (j > i implies (unsigned char*)__dest[j] == old((unsigned char*)__dest[j]))));
 */
void *memccpy (void *__restrict __dest, const void *__restrict __src,
		      int __c, size_t __n);

#endif /* Misc || X/Open.  */

/*$
 * requires: size(__s) >= __n;
 * assigns: ((unsigned char*)__s)[0, __n - 1];
 * ensures: forall int i in [0, __n - 1]: (unsigned char*)__s[i] == __c;
 * ensures: return == __s;
 */
void *memset (void *__s, int __c, size_t __n);

/*$
 * requires: size(__s1) >= __n;
 * requires: size(__s2) >= __n;
 *
 * case "equal":
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s1[i] == (unsigned char*)__s2[i];
 *   ensures: return == 0;
 *
 * case "notequal":
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s1[i] != (unsigned char*)__s2[i];
 *   ensures: exists int i in [0, __n - 1]:
 *            ((forall int j in [0, __n - 1]: 
 *               (j < i implies (unsigned char*)__s1[j] == (unsigned char*)__s2[j])) and
 *             (unsigned char*)__s1[i] != (unsigned char*)__s2[i] and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] > 0 implies return > 0) and
 *             ((unsigned char*)__s1[i] - (unsigned char*)__s2[i] < 0 implies return < 0));
 */
int memcmp (const void *__s1, const void *__s2, size_t __n);

/*$
 * requires: size(__s) >= __n;
 *
 * case "found":
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s[i] == __c;
 *   ensures: exists int i in [0, __n - 1]:
 *            ((unsigned char*)__s[i] == __c and
 *             (forall int j in [0, __n - 1]: (j < i implies (unsigned char*)__s[i] != __c)) and
 *             return == (unsigned char*)__s + i);
 *
 * case "notfound":
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s[i] != __c;
 *   ensures: return == _NULL;
 */
void *memchr (const void *__s, int __c, size_t __n);

#ifdef __USE_GNU

/*$
 * requires: size(__s) >= __n;
 *
 * case "found":
 *   assumes: exists int i in [0, __n - 1]: (unsigned char*)__s[i] == __c;
 *   ensures: exists int i in [0, __n - 1]:
 *            ((unsigned char*)__s[i] == __c and
 *             (forall int j in [0, __n - 1]: (j > i implies (unsigned char*)__s[i] != __c)) and
 *             return == (unsigned char*)__s + i);
 *
 * case "notfound":
 *   assumes: forall int i in [0, __n - 1]: (unsigned char*)__s[i] != __c;
 *   ensures: return == _NULL;
 */
void *memrchr (const void *__s, int __c, size_t __n);

#endif

/*$
 * // TODO: check overlap
 * requires: exists int i in [0, size(__src) - 1]: 
 *          (__src[i] == 0 and size(__dest) >= i);
 * assigns: __dest[0, size(__dest) - 1];
 * ensures: exists int i in [0, size(__src) - 1]: 
 *          (__src[i] == 0 and
 *           __dest[i] == 0 and
 *           forall int j in [0, size(__dest) - 1]: 
 *           ((j < i implies __src[j] != 0) and
 *            (j < i implies __dest[j] == __src[i]) and
 *            (j > i implies __dest[j] == old(__dest[j]))));
 * ensures: return == __dest;
 */
char *strcpy (char *__restrict __dest, const char *__restrict __src);

/*$
 * // TODO: check overlap
 * requires: (size(__src) >= __n and size(__dest) >= __n) or 
 *           (exists int i in [0, __n - 1]: (__src[i] == 0 and size(__dest) >= i));
 *
 * case "zero":
 *   assumes: exists int i in [0, __n - 1]: __src[i] == 0;
 *   assigns: __dest[0, size(__dest) - 1];
 *   ensures: exists int i in [0, __n - 1]: 
 *          (__src[i] == 0 and
 *           __dest[i] == 0 and
 *           forall int j in [0, size(__dest) - 1]: 
 *           ((j < i implies __src[j] != 0) and
 *            (j < i implies __dest[j] == __src[i]) and
 *            (j > i implies __dest[j] == old(__dest[j]))));
 *   ensures: return == __dest;
 * 
 * case "nozero":
 *   assumes: forall int i in [0, __n - 1]: __src[i] != 0;
 *   assigns: __dest[0, __n - 1];
 *   ensures: forall int i in [0, __n - 1]: __dest[i] == __src[i];
 *   ensures: return == __dest;
 */
char *strncpy (char *__restrict __dest,
               const char *__restrict __src, size_t __n);

/*$
 * // TODO: check overlap
 * requires: exists int s in [0, size(__src) - 1]:
 *           exists int d in [0, size(__dest) - 1]:
 *           (__src[s] == 0 and 
 *            __dest[d] == 0 and 
 *            size(__dest) >= s + d + 1);
 * assigns: __dest[0, size(__dest) - 1];
 * ensures: exists int s in [0, size(__src) - 1]:
 *          exists int d in [0, size(__dest) - 1]:
 *          (__src[s] == 0 and 
             old(__dest[d] == 0) and
 *           (forall int i in [0, size(__dest) - 1]:
 *            ((i < s implies __src[i] != 0) and
 *             (i < d implies old(__dest[i]) != 0) and
 *             (i < d implies __dest[i] == old(__dest[i])) and
 *             ((d <= i and i <= s + d) implies __dest[i] == __src[i - d]) and
 *             (i > s + d implies __dest[i] == old(__dest[i])))));
 * ensures: return == __dest;
 */
char *strcat (char *__restrict __dest, const char *__restrict __src);

/*$
 * // TODO: check overlap
 * requires: exists int s in [0, size(__src) - 1]:
 *           exists int d in [0, size(__dest) - 1]:
 *           ((s == __n or __src[s] == 0) and
 *             __dest[d] == 0 and 
 *             size(__dest) >= s + d + 1);
 * assigns: __dest[0, size(__dest) - 1];
 * ensures: exists int s in [0, size(__src) - 1]:
 *          exists int d in [0, size(__dest) - 1]:
 *          ((s == __n or __src[s] == 0) and 
             old(__dest[d] == 0) and
 *           (forall int i in [0, size(__dest) - 1]:
 *            ((i < s implies __src[i] != 0) and
 *             (i < d implies old(__dest[i]) != 0) and
 *             (i < d implies __dest[i] == old(__dest[i])) and
 *             ((d <= i and i <= s + d) implies __dest[i] == __src[i - d]) and
 *             (i > s + d implies __dest[i] == old(__dest[i])))));
 * ensures: return == __dest;
 */
char *strncat (char *__restrict __dest, const char *__restrict __src,
               size_t __n);

/*$
 * requires: exists int i in [0, size(__s1) - 1]: __s1[i] == 0;
 * requires: exists int i in [0, size(__s2) - 1]: __s2[i] == 0;
 *
 * case "equal":
 *   assumes: exists int l in [0, size(__s1) - 1]:
 *            (__s1[l] == 0 and
 *             (forall int i in [0, size(__s1) - 1]:
 *              (i <= l implies __s1[i] == __s2[i])));
 *   ensures: return == 0;
 *
 * case "notequal":
 *   assumes: exists int l in [0, size(__s1) - 1]:
 *            (__s1[l] != __s2[l] and
 *             (forall int i in [0, size(__s1) - 1]:
 *              (i < l implies (__s1[i] != 0 and __s2[i] != 0))));
 *   assumes: exists int l in [0, size(__s1) - 1]: __s1[l] != __s2[l];
 *   ensures: exists int l in [0, size(__s1) - 1]:
 *            (__s1[l] != __s2[l] and
 *             (forall int i in [0, size(__s1) - 1]:
 *              (i < l implies __s1[i] == __s2[i])) and
 *             ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] > 0 implies return > 0) and
 *             ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] < 0 implies return < 0));
 */
int strcmp (const char *__s1, const char *__s2);

/*$
 * requires: size(__s1) >= __n or exists int i in [0, size(__s1) - 1]: __s1[i] == 0;
 * requires: size(__s2) >= __n or exists int i in [0, size(__s2) - 1]: __s2[i] == 0;
 *
 * case "equal":
 *   assumes: exists int l in [0, __n - 1]:
 *            ((l == __n - 1 or __s1[l] == 0) and
 *             (forall int i in [0, __n - 1]:
 *              (i <= l implies __s1[i] == __s2[i])));
 *   ensures: return == 0;
 *
 * case "notequal":
 *   assumes: exists int l in [0, __n - 1]:
 *            (__s1[l] != __s2[l] and
 *             (forall int i in [0, __n - 1]:
 *              (i < l implies (__s1[i] != 0 or __s2[i] != 0))));
 *   ensures: exists int l in [0, __n - 1]:
 *            (__s1[l] != __s2[l] and
 *             (forall int i in [0, __n - 1]:
 *              (i < l implies __s1[i] == __s2[i])) and
 *             ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] > 0 implies return > 0) and
 *             ((unsigned char*)__s1[l] - (unsigned char*)__s2[l] < 0 implies return < 0));
 */
int strncmp (const char *__s1, const char *__s2, size_t __n);

/*$
 * requires: exists int i in [0, size(__s1) - 1]: __s1[i] == 0;
 * requires: exists int i in [0, size(__s2) - 1]: __s2[i] == 0;
 * assigns:  _errno;
 */
int strcoll (const char *__s1, const char *__s2);

/*$
 * requires: exists int i in [0, size(__src) - 1]: __src[i] == 0;
 * assigns: __dest[0, __n - 1];
 * assigns: _errno;
 * ensures: return < __n implies __dest[return] == 0;
 */
size_t strxfrm (char *__restrict __dest,
                const char *__restrict __src, size_t __n);

#ifdef __USE_XOPEN2K8

/*$
 * requires: exists int i in [0, size(__s1) - 1]: __s1[i] == 0;
 * requires: exists int i in [0, size(__s2) - 1]: __s2[i] == 0;
 * assigns:  _errno;
 */
int strcoll_l (const char *__s1, const char *__s2, locale_t __l);

/*$
 * requires: exists int i in [0, size(__src) - 1]: __src[i] == 0;
 * assigns: __dest[0, __n - 1];
 * assigns: _errno;
 * ensures: return < __n implies __dest[return] == 0;
 */
size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
                  locale_t __l);

#endif

#if (defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2))

/*$
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "success":
 *   local: char* r = new Memory;
 *   ensures: return == r;
 *   ensures: exists int l in [0, size(__s) - 1]: 
 *            (__s[l] == 0 and
 *             r[l] == 0 and
 *             size(return) == l + 1 and
 *             (forall int i in [0, size(__s) - 1]: 
 *              ((i < l implies __s[i] != 0) and
 *               (i < l implies r[i] == __s[i]))));
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
char *strdup (const char *__s);

#endif

#if defined __USE_XOPEN2K8 || __GLIBC_USE (LIB_EXT2)

/*$
 * requires: size(__s) >= __n or exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "success":
 *   local: char* r = new Memory;
 *   ensures: return == r;
 *   ensures: exists int l in [0, __n]: 
 *            ((l == __n or __s[l] == 0) and
 *             r[l] == 0 and
 *             size(return) == l + 1 and
 *             (forall int i in [0, __n]: 
 *              ((i < l implies __s[i] != 0) and
 *               (i < l implies r[i] == __s[i]))));
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
char *strndup (const char *__s, size_t __n);

#endif

/*$
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "found":
 *   assumes: exists int i in [0, size(__s) - 1]:
 *            (__s[i] == __c and
 *             (forall int j in [0, size(__s) - 1]:
 *              (i < j implies __s[j] != 0)));
 *   ensures: exists int i in [0, size(__s) - 1]:
 *            (__s[i] == __c and
 *             return == __s + i and
 *             (forall int j in [0, size(__s) - 1]:
 *              (i < j implies (__s[j] != 0 and __s[j] != __c))));
 *
 * case "notfound":
 *   assumes: exists int i in [0, size(__s) - 1]:
 *            (__s[i] == 0 and
 *             (forall int j in [0, size(__s) - 1]:
 *              (i < j implies (__s[j] != 0 and __s[j] != __c))));
 *   ensures: return == _NULL;
 */
char *strchr (const char *__s, int __c);

/*$
 * requires: exists int i in [0, size(__s) - 1]: __s[i] == 0;
 *
 * case "found":
 *   assumes: exists int i in [0, size(__s) - 1]:
 *            (__s[i] == __c and
 *             (forall int j in [0, size(__s) - 1]:
 *              (i < j implies __s[j] != 0)));
 *   ensures: exists int l in [0, size(__s) - 1]:
 *            (exists int i in [0, size(__s) - 1]:
 *             (__s[i] == __c and
 *              __s[l] == 0 and
 *              i <= l and
 *              return == __s + i and
 *              forall int j in [0, size(__s) - 1]:
 *              ((j < i implies __s[j] != 0) and
 *               ((i < j and j < l) implies __s[j] != __c))));
 *
 * case "notfound":
 *   assumes: exists int i in [0, size(__s) - 1]:
 *            (__s[i] == 0 and
 *             (forall int j in [0, size(__s) - 1]:
 *              (i < j implies (__s[j] != 0 and __s[j] != __c))));
 *   ensures: return == _NULL;
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

char *strerror_l (int __errnum, locale_t __l);

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
