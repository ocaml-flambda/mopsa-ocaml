/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <assert.h>
#include "mopsa_libc_utils.h"


/*$
 * requires: valid_string(__assertion);
 * requires: valid_string(__file);
 * requires: valid_string(__function);
 * ensures : 1 == 0;
 */
void __assert_fail (const char *__assertion, const char *__file,
                    unsigned int __line, const char *__function);

/*$
 * requires: valid_string(__file);
 * requires: valid_string(__function);
 * ensures: 1 == 0;
 */
void __assert_perror_fail (int __errnum, const char *__file,
                           unsigned int __line, const char *__function);

/*$
 * requires: valid_string(__assertion);
 * requires: valid_string(__file);
 * ensures: 1 == 0;
 */
void __assert (const char *__assertion, const char *__file, int __line);


