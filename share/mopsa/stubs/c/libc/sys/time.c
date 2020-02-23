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

#include <sys/time.h>

long int _last_time;

/*$$$
 * assigns: _last_time;
 * ensures: _last_time' in [0, 2000000000]; // we are somewhere between 1970 and 2030
 */

/*$
 * requires: __tz == NULL;
 * requires: valid_ptr(__tv);
 * assigns: _last_time;
 * assigns: __tv->tv_sec;
 * assigns: __tv->tv_usec;
 * ensures: _last_time' in [_last_time + 1, 2000000000];
 * ensures: (__tv->tv_sec)' == _last_time';
 * ensures: (__tv->tv_usec)' in [0, 1000000];
 * ensures: return == 0;
 */
int gettimeofday (struct timeval *__restrict __tv, __timezone_ptr_t __tz);
