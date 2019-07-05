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


/*$
 * local: int fd = new FileDescriptor;
 * ensures: return == fd;
 */
int socket (int __domain, int __type, int __protocol);



/*$
 * requires: __fd in FileDescriptor;
 */
int connect (int __fd, const struct sockaddr * __addr, socklen_t __len);


/*$
 * requires: __fd in FileDescriptor;
 * requires: size(__buf) >= __n;
 * assigns: ((char*)__buf)[0, __n - 1];
 */
ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);
