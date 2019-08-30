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
#include <inttypes.h>
#include <stdint.h>
#include "mopsa_libc_utils.h"


// FIXME: INTMAX_MIN can not be used in stubs since it is defined with the
//        macro __INT64_C that has a paremeter, which is not supported
static const intmax_t _INTMAX_MIN = INTMAX_MIN;

/*$
 * // requires: __n > _INTMAX_MIN;
 * ensures:  (__n >= 0 implies return == __n) and
 *           (__n < 0 implies return == __n);
 */
intmax_t imaxabs (intmax_t __n);

/*$
 * requires: __denom != 0;
 * ensures:  (return.quot == __numer / __denom) and 
 *           (return.rem == __numer % __denom);
 */
imaxdiv_t imaxdiv (intmax_t __numer, intmax_t __denom);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
intmax_t strtoimax (const char *__restrict __nptr,
                    char **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
uintmax_t strtoumax (const char *__restrict __nptr,
                     char ** __restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
intmax_t wcstoimax (const __gwchar_t *__restrict __nptr,
                    __gwchar_t **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 * requires: __base == 0 or (__base >= 2 and __base <= 36);
 *
 * case "with_endptr" {
 *   assumes: __endptr != NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr < __nptr + size(__nptr);
 * }
 *
 * case "without_endptr" {
 *   assumes: __endptr == NULL;
 *   assigns:  _errno;
 * }
 */
uintmax_t wcstoumax (const __gwchar_t *__restrict __nptr,
                     __gwchar_t ** __restrict __endptr, int __base);

/*
  omitting functions in #ifdef __USE_EXTERN_INLINES
 */
