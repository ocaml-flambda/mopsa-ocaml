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

/* Stubs for the extension of stdio as found in Sun's Solaris OS */

#include <stdio_ext.h>
#include <limits.h>

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
size_t __fbufsize (FILE *__fp);


/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __freading (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __fwriting (FILE *__fp);


/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __freadable (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __fwritable (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __flbf (FILE *__fp);


/*$
 * requires: __fp in File;
 */
void __fpurge (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
size_t __fpending (FILE *__fp);

/*$
 * // emtpy stub
 */
void _flushlbf (void);


/*$
 * requires: __fp in File;
 * ensures:  return == FSETLOCKING_INTERNAL or
 *           return == FSETLOCKING_BYCALLER;
 */
int __fsetlocking (FILE *__fp, int __type);
