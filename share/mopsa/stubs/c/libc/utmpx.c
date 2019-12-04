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

/* Stubs for functions provided by utmpx.h */

#include <utmpx.h>

/*$
 * warn: "unsupported stub";
 */
void setutxent (void);

/*$
 * warn: "unsupported stub";
 */
void endutxent (void);

/*$
 * warn: "unsupported stub";
 */
struct utmpx *getutxent (void);

/*$
 * warn: "unsupported stub";
 */
struct utmpx *getutxid (const struct utmpx *__id);

/*$
 * warn: "unsupported stub";
 */
struct utmpx *getutxline (const struct utmpx *__line);

/*$
 * warn: "unsupported stub";
 */
struct utmpx *pututxline (const struct utmpx *__utmpx);


/*$
 * warn: "unsupported stub";
 */
int utmpxname (const char *__file);

/*$
 * warn: "unsupported stub";
 */
void updwtmpx (const char *__wtmpx_file,
		      const struct utmpx *__utmpx);


/*$
 * warn: "unsupported stub";
 */
void getutmp (const struct utmpx *__utmpx,
		     struct utmp *__utmp);

/*$
 * warn: "unsupported stub";
 */
void getutmpx (const struct utmp *__utmp, struct utmpx *__utmpx);
