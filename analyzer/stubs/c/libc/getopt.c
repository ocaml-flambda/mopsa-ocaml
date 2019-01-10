/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/

/*
 * Stubs for getopt.h
 * Based on header from glibc-2.27-r6
 */

#include <getopt.h>

char *optarg;
int optind = 1;
int opterr;
int optopt;

/*$
 * requires: forall int i in [0, size(___argv) - 1]:
 *             valid_string(___argv[i]);
 * requires: valid_string(__shortopts);
 * requires: optind > 0;
 *
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * // TODO assigns: ___argv[*][*];
 *
 * ensures: return in [-1, 255];
 */
int getopt (int ___argc, char *const *___argv, const char *__shortopts);



/*$
 * requires: forall int i in [0, size(___argv) - 1]: valid_string(___argv[i]);
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, sizeof(struct option) - 1]: ___argv[size(___argv) - 1][i] == 0;
 * requires: optind > 0;
 *
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * // TODO assigns: ___argv[*][*];
 *
 * ensures: return in [-1, 255];
 */
int getopt_long (int ___argc, char *const *___argv, const char *__shortopts,
                 const struct option *__longopts, int *__longind);


/*$
 * requires: forall int i in [0, size(___argv) - 1]: valid_string(___argv[i]);
 * requires: valid_string(__shortopts);
 * requires: forall int i in [0, sizeof(struct option) - 1]: ___argv[size(___argv) - 1][i] == 0;
 * requires: optind > 0;
 *
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 * assigns: optarg;
 * // TODO assigns: ___argv[*][*];
 *
 * ensures: return in [-1, 255];
 */
int getopt_long_only (int ___argc, char *const *___argv, const char *__shortopts,
                      const struct option *__longopts, int *__longind);
