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
  based on header from glibc-2.29-r7
*/
#include <stdarg.h>
#include <wchar.h>
#include "mopsa_libc_utils.h"


/*$$
 * predicate no_overlap_wide(s1, n1, s2, n2):
 *   base(s1) != base(s2) or
 *   not (s1 <= s2 + n2) or
 *   not (s2 <= s1 + n1);
 */


/*$
 * requires: valid_wide_string(__src);
 * local: size_t src_len = wcslen(__src);
 * requires: no_overlap_wide(__src, src_len,__dest, src_len);
 * requires: valid_wchars(__dest, src_len);
 * assigns: __dest[0, src_len];
 * ensures: forall int i in [0, src_len]: (__dest[i])' == __src[i];
 * ensures: return == __dest;
 */
wchar_t *wcscpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

/*$
 * local: size_t src_nlen = wcsnlen(__src, __n);
 * requires: no_overlap_wide(__src, src_nlen, __dest, src_nlen);
 * requires: valid_wchars(__dest, __n);
 * assigns: __dest[0, __n - 1];
 * ensures: forall int i in [0, (int)src_nlen - 1]: (__dest[i])' == __src[i];
 * ensures: forall int i in [src_nlen, __n - 1]: (__dest[i])' == 0;
 * ensures: return == __dest;
 */
wchar_t *wcsncpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

/*$
 * requires: valid_wide_string(__src);
 * requires: valid_wide_string(__dest);
 * local: size_t src_len = wcslen(__src);
 * local: size_t dest_len = wcslen(__dest);
 * requires: no_overlap_wide(__src, src_len, __dest, dest_len + src_len);
 * requires: valid_wchars(__dest, dest_len + src_len);
 * assigns: __dest[0, dest_len + src_len];
 * ensures: forall int i in [0, (int)dest_len - 1]: (__dest[i])' == __dest[i];
 * ensures: forall int i in [dest_len, dest_len + src_len]: (__dest[i])' == __src[i - dest_len];
 * ensures: return == __dest;
 */
wchar_t *wcscat (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

/*$
 * requires: valid_wide_string(__dest);
 * local: size_t src_nlen = wcsnlen(__src, __n);
 * local: size_t dest_len = wcslen(__dest);
 * requires: no_overlap_wide(__src, src_nlen, __dest, dest_len + src_nlen);
 * requires: valid_wchars(__dest, dest_len + src_nlen);
 * assigns: __dest[0, dest_len + src_nlen];
 * ensures: forall int i in [0, (int)dest_len - 1]: (__dest[i])' == __dest[i];
 * ensures: forall int i in [dest_len, dest_len + src_nlen]: (__dest[i])' == __src[i - dest_len];
 * ensures: return == __dest;
 */
wchar_t *wcsncat (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscmp (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: valid_ptr(__s1);
 * requires: valid_ptr(__s2);
 * requires: bytes(__s1) < offset(__s1) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s1);
 * requires: bytes(__s2) < offset(__s2) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s2);
 */
int wcsncmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscasecmp (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: bytes(__s1) < offset(__s1) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s1);
 * requires: bytes(__s2) < offset(__s2) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s2);
 */
int wcsncasecmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 *
 */
int wcscasecmp_l (const wchar_t *__s1, const wchar_t *__s2, locale_t __loc);

/*$
 * requires: bytes(__s1) < offset(__s1) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s1);
 * requires: bytes(__s2) < offset(__s2) + __n * sizeof_type(wchar_t) implies valid_wide_string(__s2);
 */
int wcsncasecmp_l (const wchar_t *__s1, const wchar_t *__s2, size_t __n, locale_t __loc);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 * assigns:  _errno;
 */
int wcscoll (const wchar_t *__s1, const wchar_t *__s2);

/*$
 * requires: valid_wide_string(__s2);
 * assigns: _errno;
 * 
 * case "zero" {
 *   assumes: __n == 0;
 *   requires: valid_ptr(__s1) or __s1 == NULL;
 * }
 *
 * case "non-zero" {
 *   assumes: __n > 0;
 *   requires: valid_wchars(__s1, __n);
 *   requires: no_overlap_wide(__s1, __n, __s2, __n);
 *   assigns: __s1[0, __n - 1];
 *   ensures: return < __n implies (__s1[return])' == 0;
 * }
 */
size_t wcsxfrm (wchar_t *__restrict __s1, const wchar_t *__restrict __s2, size_t __n);

/*$
 * requires: valid_wide_string(__s1);
 * requires: valid_wide_string(__s2);
 * assigns:  _errno;
 */
int wcscoll_l (const wchar_t *__s1, const wchar_t *__s2, locale_t __loc);

/*$
 * requires: valid_wide_string(__s2);
 * assigns: _errno;
 * 
 * case "zero" {
 *   assumes: __n == 0;
 *   requires: valid_ptr(__s1) or __s1 == NULL;
 * }
 *
 * case "non-zero" {
 *   assumes: __n > 0;
 *   requires: valid_wchars(__s1, __n);
 *   requires: no_overlap_wide(__s1, __n, __s2, __n);
 *   assigns: __s1[0, __n - 1];
 *   ensures: return < __n implies (__s1[return])' == 0;
 * }
 */
size_t wcsxfrm_l (wchar_t *__s1, const wchar_t *__s2, size_t __n, locale_t __loc);

/*$
 * local: size_t len = wcslen(__s);
 *
 * case "success" {
 *   local: wchar_t* r = new Memory;
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
wchar_t *wcsdup (const wchar_t *__s);

/*$
 * requires: valid_wide_string(__wcs);
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists int i in [0, len - 1]: ( 
 *             __wcs[i] == __wc and 
 *             (forall int j in [0, i - 1]: __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, len - 1]: __wcs[i] != __wc;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wcschr (const wchar_t *__wcs, wchar_t __wc);

/*$
 * requires: valid_wide_string(__wcs);
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists int i in [0, len - 1]: ( 
 *             __wcs[i] == __wc and 
 *             (forall int j in [i + 1, len - 1]: __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, len - 1]: __wcs[i] != __wc;
 *   ensures: return == NULL;
 * }
 */
wchar_t *wcsrchr (const wchar_t *__wcs, wchar_t __wc);

#ifdef __USE_GNU

/*$
 * requires: valid_wide_string(__wcs);
 * local: size_t len = wcslen(__wcs);
 *
 * case "found" {
 *   ensures:  exists int i in [0, len - 1]: ( 
 *             __wcs[i] == __wc and 
 *             (forall int j in [i + 1, len - 1]: __wcs[j] != __wc) and
 *             return == __wcs + i 
 *          );
 * }
 *
 * case "notfound" {
 *   assumes: forall int i in [0, len - 1]: __wcs[i] != __wc;
 *   ensures: return == __wcs + len;
 * }
 */
wchar_t *wcsrchrnul (const wchar_t *__wcs, wchar_t __wc);

#endif

/*$
 * requires: valid_wide_string(__wcs);
 * requires: valid_wide_string(__reject);
 * local: size_t len = wcslen(__wcs);
 * ensures: return >= 0 and return <= len;
 */
size_t wcscspn (const wchar_t *__wcs, const wchar_t *__reject);

/*$
 * requires: valid_wide_string(__wcs);
 * requires: valid_wide_string(__accept);
 * local: size_t len = wcslen(__wcs);
 * ensures: return >= 0 and return <= len;
 */
size_t wcsspn (const wchar_t *__wcs, const wchar_t *__accept);

/*$
 * requires: valid_wide_string(__wcs);
 * requires: valid_wide_string(__accept);
 * local: size_t len = wcslen(__wcs);
 * ensures: return == NULL or (exists int i in [0, len - 1]: return == __wcs + i);
 */
wchar_t *wcspbrk (const wchar_t *__wcs, const wchar_t *__accept);

/*$
 * requires: valid_wide_string(__haystack);
 * requires: valid_wide_string(__needle);
 * local: size_t len1 = wcslen(__haystack);
 * local: size_t len2 = wcslen(__needle);
 * ensures: return == NULL or (exists int i in [0, len1 - len2]: return == __haystack + i);
 */
wchar_t *wcsstr (const wchar_t *__haystack, const wchar_t *__needle);
     
wchar_t *wcstok (wchar_t *__restrict __s,
                 const wchar_t *__restrict __delim,
                 wchar_t **__restrict __ptr);

/*$
 * requires: valid_wide_string(__s);
 * ensures:  exists int i in [0, ((bytes(__s) - offset(__s)) / sizeof_type(wchar_t)) - 1]: ( 
 *             __s[i] == 0 and 
 *             (forall int j in [0, i - 1]: __s[j] != 0) and
 *             return == i 
 *          );
 */
size_t wcslen (const wchar_t *__s);

/*$
 * requires: valid_ptr(__s);
 *
 * case "string-smaller-than-maxlen" {
 *   assumes: bytes(__s) - offset(__s) <= __maxlen * sizeof_type(wchar_t);
 *   requires: valid_wide_string(__s);
 *   local: size_t len = wcslen(__s);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen-with-zero" {
 *   assumes: bytes(__s) - offset(__s) >= (__maxlen + 1) * sizeof_type(wchar_t);
 *   assumes: exists int i in [0, __maxlen]: __s[i] == 0;
 *   local: size_t len = wcslen(__s);
 *   ensures: return == len;
 * }
 *
 * case "string-bigger-than-maxlen-without-zero" {
 *   assumes: bytes(__s) - offset(__s) >= (__maxlen + 1) * sizeof_type(wchar_t);
 *   assumes: forall int i in [0, __maxlen]: __s[i] != 0;
 *   ensures: return == __maxlen;
 * }
 */
size_t wcsnlen (const wchar_t *__s, size_t __maxlen);

wchar_t *wmemchr (const wchar_t *__s, wchar_t __c, size_t __n);

int wmemcmp (const wchar_t *__s1, const wchar_t *__s2, size_t __n);

wchar_t *wmemcpy (wchar_t *__restrict __s1, const wchar_t *__restrict __s2, size_t __n);

wchar_t *wmemmove (wchar_t *__s1, const wchar_t *__s2, size_t __n);

wchar_t *wmemset (wchar_t *__s, wchar_t __c, size_t __n);

wint_t btowc (int __c);

int wctob (wint_t __c);

/*$
 * requires: __ps == NULL || valid_ptr(__ps);
 */
int mbsinit (const mbstate_t *__ps);

size_t mbrtowc (wchar_t *__restrict __pwc,
                const char *__restrict __s, size_t __n,
                mbstate_t *__restrict __p) ;

size_t wcrtomb (char *__restrict __s, wchar_t __wc, mbstate_t *__restrict __ps);

size_t __mbrlen (const char *__restrict __s, size_t __n, mbstate_t *__restrict __ps);

size_t mbrlen (const char *__restrict __s, size_t __n, mbstate_t *__restrict __ps);

size_t mbsrtowcs (wchar_t *__restrict __dst,
                  const char **__restrict __src, size_t __len,
                  mbstate_t *__restrict __ps);

size_t wcsrtombs (char *__restrict __dst,
                  const wchar_t **__restrict __src, size_t __len,
                  mbstate_t *__restrict __ps);

size_t mbsnrtowcs (wchar_t *__restrict __dst,
                   const char **__restrict __src, size_t __nmc,
                   size_t __len, mbstate_t *__restrict __ps);

size_t wcsnrtombs (char *__restrict __dst,
                   const wchar_t **__restrict __src,
                   size_t __nwc, size_t __len,
                   mbstate_t *__restrict __ps);

/*$
 * requires: valid_wide_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
double wcstod (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: valid_wide_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
float wcstof (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: valid_wide_string(__nptr);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
long double wcstold (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr);

/*$
 * requires: valid_wide_string(__nptr);
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
long int wcstol (const wchar_t *__restrict __nptr, wchar_t **__restrict __endptr, int __base);

/*$
 * requires: valid_wide_string(__nptr);
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
unsigned long int wcstoul (const wchar_t *__restrict __nptr,
                           wchar_t **__restrict __endptr, int __base);
     
/*$
 * requires: valid_wide_string(__nptr);
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
long long int wcstoll (const wchar_t *__restrict __nptr,
                       wchar_t **__restrict __endptr, int __base);

/*$
 * requires: valid_wide_string(__nptr);
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   requires: valid_ptr(__endptr);
 *   local: size_t len = wcslen(__nptr);
 *   assigns: *__endptr;
 *   assigns: _errno;
 *   ensures: exists int i in [0, len]: (*__endptr)' == __nptr + i;
 * }
 *
 *  case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 *  }
 */
unsigned long long int wcstoull (const wchar_t *__restrict __nptr,
                                 wchar_t **__restrict __endptr,
                                 int __base);

wchar_t *wcpcpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src);

wchar_t *wcpncpy (wchar_t *__restrict __dest, const wchar_t *__restrict __src, size_t __n);

FILE *open_wmemstream (wchar_t **__bufloc, size_t *__sizeloc);

int fwide (FILE *__fp, int __mode);

int fwprintf (FILE *__restrict __stream, const wchar_t *__restrict __format, ...);

int wprintf (const wchar_t *__restrict __format, ...);

int swprintf (wchar_t *__restrict __s, size_t __n, const wchar_t *__restrict __format, ...);

int vfwprintf (FILE *__restrict __s,
               const wchar_t *__restrict __format,
               va_list __arg);

int vwprintf (const wchar_t *__restrict __format, va_list __arg);

int vswprintf (wchar_t *__restrict __s, size_t __n,
               const wchar_t *__restrict __format,
               va_list __arg);

int fwscanf (FILE *__restrict __stream, const wchar_t *__restrict __format, ...);

int wscanf (const wchar_t *__restrict __format, ...);

int swscanf (const wchar_t *__restrict __s, const wchar_t *__restrict __format, ...);

int fwscanf (FILE *__restrict __stream, const wchar_t *__restrict __format, ...);

int wscanf (const wchar_t *__restrict __format, ...);
                                                         ;
int swscanf (const wchar_t *__restrict __s, const wchar_t *__restrict __format, ...);

int vfwscanf (FILE *__restrict __s,
              const wchar_t *__restrict __format,
              va_list __arg);

int vwscanf (const wchar_t *__restrict __format,
             va_list __arg);

int vswscanf (const wchar_t *__restrict __s,
              const wchar_t *__restrict __format,
              va_list __arg);
     
int vfwscanf (FILE *__restrict __s, const wchar_t *__restrict __format, va_list __arg);

int vwscanf (const wchar_t *__restrict __format, va_list __arg);
                                                         ;
int vswscanf (const wchar_t *__restrict __s, const wchar_t *__restrict __format, va_list __arg);

wint_t fgetwc (FILE *__stream);
wint_t getwc (FILE *__stream);

wint_t getwchar (void);

wint_t fputwc (wchar_t __wc, FILE *__stream);
wint_t putwc (wchar_t __wc, FILE *__stream);

wint_t putwchar (wchar_t __wc);

wchar_t *fgetws (wchar_t *__restrict __ws, int __n, FILE *__restrict __stream);

int fputws (const wchar_t *__restrict __ws, FILE *__restrict __stream);

wint_t ungetwc (wint_t __wc, FILE *__stream);

size_t wcsftime (wchar_t *__restrict __s, size_t __maxsize,
                 const wchar_t *__restrict __format,
                 const struct tm *__restrict __tp);
