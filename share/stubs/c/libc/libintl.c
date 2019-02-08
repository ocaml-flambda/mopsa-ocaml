/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/

/*
 * Stubs for libintl.h
 * Based on header from glibc-2.27-r6
 */

#include <libintl.h>

char *_domain_name = NULL;

/*$
 * requires: valid_string(__domainname);
 *
 * case "with domain name" {
 *   assumes: __domainname != NULL;
 *   assigns: _domain_name;
 *   local: char *addr = new ReadOnlyString;
 *
 *   ensures: _domain_name' == addr;
 *   ensures: valid_string(addr);
 *   ensures: return == addr;
 *   // TODO: invalidate _domain_name
 * }
 *
 * case "without domain name" {
 *   assumes: __domainname == NULL;
 *   requires: _domain_name != NULL;
 *   ensures: return == _domain_name;
 * }
 */
char *textdomain (const char *__domainname);


/*$
 * requires: valid_string(__domainname);
 *
 * case "with directory name" {
 *  assumes: __dirname != NULL;
 *  local: char *addr = new ReadOnlyString;
 *  ensures: return == addr;
 *  ensures: valid_string(addr);
 * }
 *
 * case "without directory name" {
 *   assumes: __dirname == NULL;
 *   warn: "calling bindtextdomain without dirname is not supported";
 * }
 */
char *bindtextdomain (const char *__domainname, const char *__dirname);

/*$
 * warn: "unsupported stub";
 */
char *bind_textdomain_codeset (const char *__domainname, const char *__codeset);


/*$
 * warn: "unsupported stub";
 */
char *gettext (const char *__msgid);

/*$
 * warn: "unsupported stub";
 */
char *dcgettext (const char *__domainname, const char *__msgid,
		 int __category);

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
