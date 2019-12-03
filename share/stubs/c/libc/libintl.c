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
 * Stubs for libintl.h
 * Based on header from glibc-2.27-r6
 */

#include <libintl.h>


/* If true, gettext returns its argument string unchanged.
   This makes it possible to check printf formats that use gettext.
 */
#define IDENTITY_GETTEXT 1


#define DOMAIN_NAME_BUF_SIZE 100

char *_domain_name_buf = NULL;

/*$
 * case "with domain name AND first call" {
 *   assumes: __domainname != NULL;
 *   assumes: _domain_name_buf == NULL;
 *   requires: valid_string(__domainname);
 *   assigns: _domain_name_buf;
 *   local: char *addr = new ReadOnlyString;
 *   ensures: size(addr) == DOMAIN_NAME_BUF_SIZE;
 *   ensures: _domain_name_buf' == addr;
 *   ensures: valid_string(addr);
 *   ensures: return == addr;
 * }
 *
 * case "with domain name AND later calls" {
 *   assumes: __domainname != NULL;
 *   assumes: _domain_name_buf != NULL;
 *   requires: valid_string(__domainname);
 *   assigns: _domain_name_buf;
 *   local: char *addr = new ReadOnlyString;
 *   ensures: size(addr) == DOMAIN_NAME_BUF_SIZE;
 *   ensures: _domain_name_buf' == addr;
 *   ensures: valid_string(addr);
 *   ensures: return == addr;
 *   free: _domain_name_buf;
 * }
 *
 * case "without domain name" {
 *   assumes: __domainname == NULL;
 *   ensures: return == _domain_name_buf;
 * }
 */
char *textdomain (const char *__domainname);


/*$
 * requires: valid_string(__domainname);
 *
 * case "with directory name AND first call" {
 *   assumes: __dirname != NULL;
 *   assumes: _domain_name_buf == NULL;
 *   assigns: _domain_name_buf;
 *   local: char *addr = new ReadOnlyString;
 *   ensures: size(addr) == DOMAIN_NAME_BUF_SIZE;
 *   ensures: _domain_name_buf' == addr;
 *   ensures: valid_string(addr);
 *   ensures: return == addr;
 * }
 *
 * case "with directory name AND later calls" {
 *   assumes: __dirname != NULL;
 *   assumes: _domain_name_buf != NULL;
 *   assigns: _domain_name_buf;
 *   local: char *addr = new ReadOnlyString;
 *   ensures: size(addr) == DOMAIN_NAME_BUF_SIZE;
 *   ensures: _domain_name_buf' == addr;
 *   ensures: valid_string(addr);
 *   ensures: return == addr;
 *   free: _domain_name_buf;
 * }
 *
 * case "without directory name" {
 *   assumes: __dirname == NULL;
 *   ensures: return == _domain_name_buf;
 * }
 */
char *bindtextdomain (const char *__domainname, const char *__dirname);

/*$
 * warn: "unsupported stub";
 */
char *bind_textdomain_codeset (const char *__domainname, const char *__codeset);

#define GETTEXT_BUF_SIZE 100
char _gettext_buf[GETTEXT_BUF_SIZE];

#if IDENTITY_GETTEXT
char *gettext (const char *__msgid) { return (char*)__msgid; }
#else
/*$
 * requires: valid_string(__msgid);
 *
 * case "translation found" {
 *   assigns: _gettext_buf[0, GETTEXT_BUF_SIZE - 1];
 *   ensures: valid_primed_string(_gettext_buf);
 *   ensures: return == _gettext_buf;
 * }
 *
 * case "translation not found" {
 *   ensures: return == _gettext_buf;
 * }
 */
char *gettext (const char *__msgid);
#endif

#if IDENTITY_GETTEXT
char *dcgettext (const char *__domainname, const char *__msgid,
		 int __category)
{ return (char*)__msgid; }
#else
/*$
 * requires: valid_string(__msgid);
 *
 * case "translation found" {
 *   assigns: _gettext_buf[0, GETTEXT_BUF_SIZE - 1];
 *   ensures: valid_primed_string(_gettext_buf);
 *   ensures: return == _gettext_buf;
 * }
 *
 * case "translation not found" {
 *   ensures: return == _gettext_buf;
 * }
 */
char *dcgettext (const char *__domainname, const char *__msgid,
		 int __category);
#endif

/*$
 * warn: "unsupported stub";
 */
char *__dcgettext (const char *__domainname, const char *__msgid,
		   int __category);

/*$
 * warn: "unsupported stub";
 */
char *ngettext (const char *__msgid1, const char *__msgid2,
		unsigned long int __n);

/*$
 * warn: "unsupported stub";
 */
char *dngettext (const char *__domainname, const char *__msgid1,
		 const char *__msgid2, unsigned long int __n);

/*$
 * warn: "unsupported stub";
 */
char *dcngettext (const char *__domainname, const char *__msgid1,
		  const char *__msgid2, unsigned long int __n, int __category);
