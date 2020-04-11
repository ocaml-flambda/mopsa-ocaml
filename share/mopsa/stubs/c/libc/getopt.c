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
 * Stubs for getopt.h
 * Based on header from glibc-2.27-r6
 */

#include <getopt.h>
#include <stddef.h> // to get NULL

char *optarg;
int optind = 1;
int opterr;
int optopt;


/*$
 * requires: optind > 0 and optind <= ___argc;
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, ___argc - 1]: valid_string(___argv[i]);
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * assigns: ___argv[0, ___argc - 1];
 * ensures: optind' in [1, ___argc];
 * ensures: forall int i in [0, ___argc - 1]: exists int j in [0, ___argc - 1]: (___argv[i])' == ___argv[j];
 * ensures: optarg' == NULL or exists int i in [0, ___argc - 1]: in_string(optarg', ___argv[i]);
 * ensures: return in [-1, 255];
 */
int getopt (int ___argc, char *const *___argv, const char *__shortopts);



/*$$
 * predicate valid_longopt(l):
 *   valid_ptr(l) and
 *   exists int i in [0, ((bytes(l) - offset(l)) / sizeof_type(struct option)) - 1]: (
 *     l[i].name == NULL and
 *     forall int j in [0, i - 1]: (
 *       valid_ptr(l[j].name) and
 *       (exists int k in [0, size(l[j].name) - offset(l[j].name) - 1]: l[j].name[k] == 0) and
 *       (l[j].flag != NULL implies valid_ptr(l[j].flag))
 *     )
 *   );
 */

/*$
 * ensures:
 *   exists int i in [0, ((bytes(l) - offset(l)) / sizeof_type(struct option)) - 1]: (
 *     l[i].name == NULL and
 *     return == i and
 *     forall int j in [0, i - 1]: l[j].name != NULL
 *   );
 */
int _mopsa_len_option (const struct option* l);

/*$
 * ensures: return in [0, i - 1];
 */
int _mopsa_pick(int i);

/*$
 * requires: optind > 0 and optind <= ___argc;
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, ___argc - 1]: valid_string(___argv[i]);
 * requires: valid_longopt(__longopts);
 * requires: __longind != NULL implies valid_ptr(__longind);
 * local: int len = _mopsa_len_option(__longopts);
 * local: int r = _mopsa_pick(len);
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * assigns: ___argv[0, ___argc - 1];
 * ensures: optind' in [1, ___argc];
 * ensures: forall int i in [0, ___argc - 1]: exists int j in [0, ___argc - 1]: (___argv[i])' == ___argv[j];
 * ensures: optarg' == NULL or exists int i in [0, ___argc - 1]: in_string(optarg', ___argv[i]);
 *
 * case "opt-ind" {
 *   assumes: __longopts[r].flag != NULL;
 *   assumes: __longind != NULL;
 *   assigns: __longopts[r].flag;
 *   assigns: *__longind;
 * }
 *
 * case "opt" {
 *   assumes: __longopts[r].flag != NULL;
 *   assumes: __longind == NULL;
 *   assigns: __longopts[r].flag;
 * }
 *
 * case "ind" {
 *   assumes: __longopts[r].flag == NULL;
 *   assumes: __longind != NULL;
 *   assigns: *__longind;
 * }
 *
 * case "none" {
 *   assumes: __longopts[r].flag == NULL;
 *   assumes: __longind == NULL;
 * }
 */
int getopt_long (int ___argc, char *const *___argv, const char *__shortopts,
                 const struct option *__longopts, int *__longind);

/*$
 * alias: getopt_long;
 */
int getopt_long_only (int ___argc, char *const *___argv, const char *__shortopts,
                      const struct option *__longopts, int *__longind);
