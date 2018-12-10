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
#include <inttypes.h>
#include "mopsa_libc_utils.h"

static const intmax_t _INTMAX_MIN = INTMAX_MIN;

/*$
 * requires: __n > _INTMAX_MIN;
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
 *
 * case "with_endptr":
 *   assumes: __endptr != _NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr <= __nptr + size(__nptr);
 *
 * case "without_endptr":
 *   assumes: __endptr == _NULL;
 *   assigns:  _errno;
 */
intmax_t strtoimax (const char *__restrict __nptr,
                    char **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 *
 * case "with_endptr":
 *   assumes: __endptr != _NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr <= __nptr + size(__nptr);
 *
 * case "without_endptr":
 *   assumes: __endptr == _NULL;
 *   assigns:  _errno;
 */
uintmax_t strtoumax (const char *__restrict __nptr,
                     char ** __restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 *
 * case "with_endptr":
 *   assumes: __endptr != _NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr <= __nptr + size(__nptr);
 *
 * case "without_endptr":
 *   assumes: __endptr == _NULL;
 *   assigns:  _errno;
 */
intmax_t wcstoimax (const __gwchar_t *__restrict __nptr,
                    __gwchar_t **__restrict __endptr, int __base);

/*$
 * //NOTE: we are more strict than the spec by requiring that __nptr is 0-terminated
 * requires: exists int i in [0, size(__nptr) - 1]: __nptr[i] == 0;
 *
 * case "with_endptr":
 *   assumes: __endptr != _NULL;
 *   assigns:  *__endptr;
 *   assigns:  _errno;
 *   ensures:  *__endptr >= __nptr and *__endptr <= __nptr + size(__nptr);
 *
 * case "without_endptr":
 *   assumes: __endptr == _NULL;
 *   assigns:  _errno;
 */
uintmax_t wcstoumax (const __gwchar_t *__restrict __nptr,
                     __gwchar_t ** __restrict __endptr, int __base);

/*
  omitting functions in #ifdef __USE_EXTERN_INLINES
 */
