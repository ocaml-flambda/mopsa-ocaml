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

#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>

/*$
 * assigns: _errno;
 * assigns: *__usage;
 * assigns: *__stat_loc;
 *
 * ensures: return >= -1;
 * ensures: (__stat_loc != NULL) implies ((*__stat_loc)' in [0,255]); // FIXME: is this sound?
 *
 * ensures: 
 *   (__usage != NULL) implies (
 *         (__usage->ru_utime.tv_sec)'  in [0, 2000000000] // 30 years :)
 *     and (__usage->ru_utime.tv_usec)' in [0, 1000000]
 *     and (__usage->ru_stime.tv_sec)'  in [0, 2000000000]
 *     and (__usage->ru_stime.tv_usec)' in [0, 1000000]
 *     and (__usage->ru_maxrss)' >= 0
 *     and (__usage->ru_minflt)' >= 0
 *     and (__usage->ru_majflt)' >= 0
 *     and (__usage->ru_inblock)' >= 0
 *     and (__usage->ru_oublock)' >= 0
 *     and (__usage->ru_nvcsw)' >= 0
 *     and (__usage->ru_nivcsw)' >= 0
 *   );
 */
pid_t wait3 (int *__stat_loc, int __options, struct rusage * __usage);
