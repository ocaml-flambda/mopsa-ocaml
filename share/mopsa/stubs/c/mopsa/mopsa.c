/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2019 The MOPSA Project.                                    */
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


#include <limits.h>


/*$$
 * predicate valid_string(s):
 *   valid_ptr(s) and
 *   size(s) >= 1 and
 *   exists int _i in [0, size(s) - offset(s) - 1]: s[_i] == 0
 * ;
 */


/*$$
 * predicate valid_primed_string(s):
 *   valid_ptr(s) and
 *   exists int _i in [0, size(s) - offset(s) - 1]: (s[_i])' == 0
 * ;
 */


/*$$
 * predicate valid_substring(s, n):
 *   valid_ptr(s) and
 *   exists int _i in [0, n - 1]: s[_i] == 0
 * ;
 */


/*$$
 * predicate valid_primed_substring(s, n):
 *   valid_ptr(s) and
 *   exists int _i in [0, n - 1]: (s[_i])' == 0
 * ;
 */


/*$$
 * predicate valid_ptr_range(p, i, j):
 *   forall int k in [i, j]: valid_ptr(p+k);
 */

/*$
 * local:   char * str = new Memory;
 * ensures: size(str) == INT_MAX;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
static char *_mopsa_new_valid_string();


/*$
 * requires: valid_ptr(p);
 */
void _mopsa_assert_valid_ptr(void *p);


/*$
 * requires: valid_string(s);
 */
void _mopsa_assert_valid_string(char *s);

/*$
 * requires: valid_substring(s,n);
 */
void _mopsa_assert_valid_substring(char *s, unsigned int n);


/*$
 * requires: valid_ptr_range(s, i, j);
 * assigns: s[i, j];
 */
void _mopsa_memrand(char *s, unsigned int i, unsigned int j);


/*$
 * requires: valid_ptr_range(s, 0, size(s) - offset(s) - 1);
 * assigns: s[0, size(s) - offset(s) - 1];
 * ensures: valid_primed_string(s);
 */
void _mopsa_strrand(char *s);

/*$
 * case "non-empty" {
 *   assumes: n >= 1;
 *   requires: valid_ptr_range(s, 0, n - 1);
 *   assigns: s[0, n - 1];
 *   ensures: valid_primed_substring(s,n);
 * }
 *
 * case "empty" {
 *   assumes: n == 0;
 * }
 */
void _mopsa_strnrand(char *s, unsigned int n);


/*$
 * requires: stream in File;
 */
void _mopsa_assert_valid_stream(void* stream);


/*$
 * requires: valid_ptr_range(s, i, j);
 * assigns: s[i, j];
 * ensures: forall unsigned int k in [i,j]: (s[k])' == c;
 */
void _mopsa_memset(char *s, char c, unsigned int i, unsigned int j);


/*$
 * requires: valid_ptr_range(dst, i, j);
 * requires: valid_ptr_range(src, i, j);
 * assigns: dst[i, j];
 * ensures: forall unsigned int k in [i,j]: (dst[k])' == src[k];
 */
void _mopsa_memcpy(char *dst, char *src, unsigned int i, unsigned int j);



/*$$
 * predicate valid_wide_string(s):
 *   valid_ptr(s) and
 *   size(s) >= 1 and
 *   exists int _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t)) - 1]: s[_i] == 0
 * ;
 */

/*$$
 * predicate valid_primed_wide_string(s):
 *   valid_ptr(s) and
 *   exists int _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t)) - 1]: (s[_i])' == 0
 * ;
 */

/*$$
 * predicate valid_bytes(s, n):
 *   valid_ptr(s) and
 *   bytes(s) >= offset(s) + n
 * ;
 */

/*$$
 * predicate valid_wchars(s, n):
 *   valid_ptr(s) and
 *   bytes(s) >= offset(s) + n * sizeof_type(wchar_t)
 * ;
 */
