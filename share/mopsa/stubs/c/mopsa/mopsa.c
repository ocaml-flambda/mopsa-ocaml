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


#include <stddef.h>
#include <limits.h>


/*$$
 * predicate null_or_valid_ptr(s):
 *   s != NULL implies valid_ptr(s);
 */



/*$$
 * predicate null_or_valid_string(s):
 *   s != NULL implies valid_string(s);
 */

/*$$
 * predicate valid_string(s):
 *   valid_ptr(s) and
 *   size(s) >= 1 and
 *   exists int _i in [0, (size(s) - offset(s)) ): s[_i] == 0
 * ;
 */

/*$$
 * predicate valid_primed_string(s):
 *   valid_ptr(s) and
 *   exists int _i in [0, (size(s) - offset(s)) ): (s[_i])' == 0
 * ;
 */


/*$$
 * predicate valid_substring(s, n):
 *   (n > 0 implies valid_ptr(s)) and
 *   exists int _i in [0, n): s[_i] == 0
 * ;
 */


/*$$
 * predicate valid_primed_substring(s, n):
 *   (n > 0 implies valid_ptr(s)) and
 *   exists int _i in [0, n): (s[_i])' == 0
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
char *_mopsa_new_valid_string();

/*$
 * local:   char * str = new ReadOnlyMemory;
 * ensures: size(str) == INT_MAX;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_readonly_string();

/*$
 * local:   char * str = new Memory;
 * ensures: size(str) == max;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_valid_string_max(size_t max);

/*$
 * local:   char * str = new ReadOnlyMemory;
 * ensures: size(str) == max;
 * ensures: valid_string(str);
 * ensures: return == str;
 */
char *_mopsa_new_readonly_string_max(size_t max);


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
 * assigns: s[0, (size(s) - offset(s)) );
 * ensures: valid_primed_string(s);
 */
void _mopsa_strrand(char *s);

/*$
 * case "non-empty" {
 *   assumes: n >= 1;
 *   requires: valid_ptr_range(s, 0, n - 1);
 *   assigns: s[0, n);
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
 * predicate in_string(x,s):
 *   exists int _i in [0, (bytes(s) - offset(s))):
 *     (x == s + _i and
 *     forall int _j in [0, _i): s[_j] != 0)
 *  ;
 */

/*$$
 * predicate null_or_valid_wide_string(s):
 *   s != NULL implies valid_wide_string(s);
 */

/*$$
 * predicate valid_wide_string(s):
 *   valid_ptr(s) and
 *   size(s) >= 1 and
 *   exists int _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))): s[_i] == 0
 * ;
 */


/*$$
 * predicate valid_wide_substring(s, n):
 *   (n > 0 implies valid_ptr(s)) and
 *   exists int _i in [0, n): s[_i] == 0
 * ;
 */

/*$$
 * predicate valid_primed_wide_string(s):
 *   valid_ptr(s) and
 *   exists int _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))): (s[_i])' == 0
 * ;
 */

/*$$
 * predicate valid_primed_wide_substring(s, n):
 *   (n > 0 implies valid_ptr(s)) and
 *   exists int _i in [0, n): (s[_i])' == 0
 * ;
 */

/*$$
 * predicate in_wide_string(x,s):
 *   exists int _i in [0, ((bytes(s) - offset(s)) / sizeof_type(wchar_t))):
 *     (x == s + _i and
 *     forall int _j in [0, _i): s[_j] != 0)
 *  ;
 */


/*$$
 * predicate valid_bytes(s, n):
 *   n > 0 implies (valid_ptr(s) and bytes(s) >= offset(s) + n)
 * ;
 */

/*$$
 * predicate null_or_valid_bytes(s, n):
 *   n > 0 implies s != NULL implies (valid_ptr(s) and bytes(s) >= offset(s) + n)
 * ;
 */


/*$$
 * predicate in_bytes(r, x, n):
 *    exists int _i in [0, n]: r == x + _i
 * ;
 */

/*$$
 * predicate valid_wchars(s, n):
 *   n > 0 implies (valid_ptr(s) and bytes(s) >= offset(s) + n * sizeof_type(wchar_t))
 * ;
 */

/*$$
 * predicate null_or_valid_wchars(s, n):
 *   n > 0 implies s != NULL implies (valid_ptr(s) and bytes(s) >= offset(s) + n * sizeof_type(wchar_t))
 * ;
 */

/*$$
 * predicate in_wchars(r, x, n):
 *    exists int _i in [0, n]: r == x + _i
 * ;
 */
