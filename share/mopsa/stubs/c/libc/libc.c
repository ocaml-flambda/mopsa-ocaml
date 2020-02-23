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
 * Entry point of the libc
 */

#include "errno.c"
#include "assert.c"
#include "stdio.c"
#include "stdio_ext.c"
#include "stdlib.c"
#include "unistd.c"
#include "string.c"
#include "getopt.c"
#include "locale.c"
#include "libintl.c"
#include "utmp.c"
#include "utmpx.c"
#include "signal.c"
#include "error.c"
#include "builtins.c"
#include "sys/socket.c"
#include "arpa/inet.c"
#include "netinet/in.c"
#include "fcntl.c"
#include "inttypes.c"
#include "langinfo.c"
#include "time.c"
#include "pwd.c"
#include "sys/resource.c"
#include "sys/utsname.c"
#include "sys/time.c"
#include "sys/wait.c"

/* TODO: include math library only when -lm is used */
#include "math.c"
