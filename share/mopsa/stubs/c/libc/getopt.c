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
 * ensures: optarg' == NULL or (exists int i in [0, ___argc - 1]: optarg' == (___argv[i])');
 * ensures: forall int i in [0, ___argc - 1]: valid_primed_string(___argv[i]);
 */
int getopt (int ___argc, char *const *___argv, const char *__shortopts);

// TODO
int getopt_long (int ___argc, char *const *___argv, const char *__shortopts,
                 const struct option *__longopts, int *__longind);

// TODO
int getopt_long_only (int ___argc, char *const *___argv, const char *__shortopts,
                      const struct option *__longopts, int *__longind);
