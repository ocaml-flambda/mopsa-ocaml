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
#include <stddef.h>
#include <assert.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_string(__assertion);
 * requires: valid_string(__file);
 * requires: valid_string(__function);
 * ensures: raise("__assert_fail called");
 * ensures : 1 == 0;
 */
void __assert_fail (const char *__assertion, const char *__file,
                    unsigned int __line, const char *__function);

/*$
 * requires: valid_string(__file);
 * requires: valid_string(__function);
 * ensures: raise("__assert_perror_fail called");
 * ensures: 1 == 0;
 */
void __assert_perror_fail (int __errnum, const char *__file,
                           unsigned int __line, const char *__function);

/*$
 * requires: valid_string(__assertion);
 * requires: valid_string(__file);
 * ensures: raise("__assert called");
 * ensures: 1 == 0;
 */
void __assert (const char *__assertion, const char *__file, int __line);

