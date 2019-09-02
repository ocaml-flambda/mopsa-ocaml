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

#include <sys/types.h>
#include <pwd.h>

#define NB_PASSWD_ENTRIES 10
struct passwd _passwd_db[NB_PASSWD_ENTRIES];

/*$
 * case "success" {
 *   local: unsigned int i = _mopsa_range_u32(0, NB_PASSWD_ENTRIES - 1);
 *   ensures: return == &(_passwd_db[i]);
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
struct passwd *getpwuid (__uid_t __uid);
