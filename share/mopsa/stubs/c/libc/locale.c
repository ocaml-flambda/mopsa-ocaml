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

#include <locale.h>
#include <limits.h>


#define LOCAL_BUF_SIZE 100

char * _local_buf;


/*$$$
 * assigns: _local_buf;
 * local: char* addr = new ReadOnlyString;
 * ensures: size(addr) == LOCAL_BUF_SIZE;
 * // TODO ensures: valid_string(addr);
 * ensures: _local_buf' == addr;
 */



/*$
 * requires: __locale != NULL implies valid_string(__locale);
 *
 * case "success" {
 *   local:   char* addr = new ReadOnlyString;
 *   assigns: _local_buf;
 *   ensures: size(addr) == LOCAL_BUF_SIZE;
 *   ensures: valid_string(addr);
 *   ensures: _local_buf' == addr;
 *   ensures: return == addr;
 *   free:    _local_buf;
 * }
 *
 * case "failure" {
 *   ensures: return == NULL;
 * }
 */
char *setlocale (int __category, const char *__locale);


struct lconv * _lconv_buf = NULL;

/*$
 * local: struct lconv * addr = new ReadOnlyMemory;
 *
 * assigns: _lconv_buf;
 *
 * ensures: size(addr) == sizeof_type(struct lconv);
 * ensures: return == addr;
 * ensures: _lconv_buf' == addr;
 *
 * case "first-allocation" {
 *   assumes: _lconv_buf == NULL;
 * }
 *
 * case "invalidate-previous-lconv" {
 *   assumes: _lconv_buf != NULL;
 *   free: _lconv_buf;
 * }
 */
struct lconv *localeconv (void);


#ifdef	__USE_XOPEN2K8


// type __locale_t has been renamed local_t starting from glibv 2.26
#ifndef local_t
#define local_t __local_t
#endif


/*$
 * warn: "unsupported stub";
 */
locale_t newlocale (int __category_mask, const char *__locale, locale_t __base);

/*$
 * warn: "unsupported stub";
 */
locale_t duplocale (locale_t __dataset);

/*$
 * warn: "unsupported stub";
 */
void freelocale (locale_t __dataset);

/*$
 * warn: "unsupported stub";
 */
locale_t uselocale (locale_t __dataset);



#endif
