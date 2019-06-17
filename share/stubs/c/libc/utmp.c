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

/* Stubs for functions provided by utmp.h */

#include <utmp.h>

/*$
 * warn: "unsupported stub";
 */
int login_tty (int __fd);


/*$
 * warn: "unsupported stub";
 */
void login (const struct utmp *__entry);


/*$
 * warn: "unsupported stub";
 */
int logout (const char *__ut_line);


/*$
 * warn: "unsupported stub";
 */
void logwtmp (const char *__ut_line, const char *__ut_name,
		     const char *__ut_host);

/*$
 * warn: "unsupported stub";
 */
void updwtmp (const char *__wtmp_file, const struct utmp *__utmp)
    ;


/*$
 * warn: "unsupported stub";
 */
int utmpname (const char *__file);


/*$
 * warn: "unsupported stub";
 */
struct utmp *getutent (void);


/*$
 * warn: "unsupported stub";
 */
void setutent (void);


/*$
 * warn: "unsupported stub";
 */
void endutent (void);


/*$
 * warn: "unsupported stub";
 */
struct utmp *getutid (const struct utmp *__id);


/*$
 * warn: "unsupported stub";
 */
struct utmp *getutline (const struct utmp *__line);


/*$
 * warn: "unsupported stub";
 */
struct utmp *pututline (const struct utmp *__utmp_ptr);


/*$
 * warn: "unsupported stub";
 */
int getutent_r (struct utmp *__buffer, struct utmp **__result);

/*$
 * warn: "unsupported stub";
 */
int getutid_r (const struct utmp *__id, struct utmp *__buffer,
		      struct utmp **__result);

/*$
 * warn: "unsupported stub";
 */
int getutline_r (const struct utmp *__line,
			struct utmp *__buffer, struct utmp **__result);
