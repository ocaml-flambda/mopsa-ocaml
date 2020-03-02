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

/* Stubs for functions in error.h */

#include <error.h>

/*$
 * ensures: 1 == 0;
 */
void error (int __status, int __errnum, const char *__format, ...);


/*$
 * ensures: 1 == 0;
 */
void error_at_line (int __status, int __errnum, const char *__fname, unsigned int __lineno, const char *__format, ...);


// TODO
void (*error_print_progname) (void);


// TODO
unsigned int error_message_count;


// TODO
int error_one_per_line;
