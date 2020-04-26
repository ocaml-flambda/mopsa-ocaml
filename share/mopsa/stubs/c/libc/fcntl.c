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

/*$
 * requires: valid_string(__file);
 * requires: __fd in FileDescriptor;
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
int openat (int __fd, const char *__file, int __oflag, ...);

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
int creat (const char *__file, mode_t __mode);

/*$
 * requires: __fd in FileDescriptor;
 */
int posix_fadvise (int __fd, off_t __offset, off_t __len, int __advise);

/*$
 * requires: __fd in FileDescriptor;
 */
int posix_fallocate (int __fd, off_t __offset, off_t __len);
