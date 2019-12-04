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
#include <fcntl.h>
#include "mopsa_libc_utils.h"


/*$
 * requires: __fd in FileDescriptor;
 * // TODO: check __cmd & handle variable arguments
 */
int fcntl (int __fd, int __cmd, ...);

/*$
 * requires: valid_string(__file);
 *
 * case "success" {
 *   local:   void* f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int open (const char *__file, int __oflag, ...);



int creat (const char *__file, mode_t __mode) {
  return open(__file, O_CREAT|O_WRONLY|O_TRUNC, __mode);
}


/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int lockf (int __fd, int __cmd, off_t __len);
