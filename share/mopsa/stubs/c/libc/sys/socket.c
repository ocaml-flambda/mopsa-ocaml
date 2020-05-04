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

/* Stubs for <sys/socket.h> */

#include <sys/socket.h>
#include "../mopsa_libc_utils.h"


/*$
 * case "safe" {
 *   local:   void *f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int socket (int __domain, int __type, int __protocol);



/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "safe" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int connect (int __fd, const struct sockaddr * __addr, socklen_t __len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "safe" {
 *   ensures: return == 0;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int bind (int __fd, const struct sockaddr *__addr, socklen_t __len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * assigns: _errno;
 * ensures: return in [-1, 0];
 */
int listen (int __fd, int __n);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 *
 * case "valid-addr" {
 *   local:   void *f = new FileRes;
 *   local:   int fd = _mopsa_register_file_resource(f);
 *   ensures: return == fd;
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int accept (int __fd, struct sockaddr *__addr, socklen_t *__restrict __addr_len);


/*$
 * local:    void* f = _mopsa_find_file_resource(__fd);
 * requires: alive_resource(f, FileRes);
 * requires: valid_bytes(__buf, __n);
 *
 * case "safe" {
 *   assigns: ((char*)__buf)[0, __n);
 *   ensures: return in [0, __n];
 * }
 *
 * case "error" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);
