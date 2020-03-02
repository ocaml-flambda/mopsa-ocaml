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

#include <time.h>
#include <errno.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_ptr(__requested_time);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "interrupted-with-remaining" {
 *   assumes: __remaining != NULL;
 *   assigns: __remaining;
 *   assigns: _errno;
 *   ensures: _errno' == EINTR;
 *   ensures: return == -1;
 * }
 *
 * case "interrupted-without-remaining" {
 *   assumes: __remaining == NULL;
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int nanosleep (const struct timespec *__requested_time,
		      struct timespec *__remaining);
