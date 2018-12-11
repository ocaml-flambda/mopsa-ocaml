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
 * requires: exists int i in [0, size(__assertion) - 1]: __assertion[i] == 0;
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 * requires: exists int i in [0, size(__function) - 1]: __function[i] == 0;
 * ensures: 1 == 0;
 */
void __assert_fail (const char *__assertion, const char *__file,
                    unsigned int __line, const char *__function);

/*$
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 * requires: exists int i in [0, size(__function) - 1]: __function[i] == 0;
 * ensures: 1 == 0;
 */
void __assert_perror_fail (int __errnum, const char *__file,
                           unsigned int __line, const char *__function);

/*$
 * requires: exists int i in [0, size(__assertion) - 1]: __assertion[i] == 0;
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 * ensures: 1 == 0;
 */
void __assert (const char *__assertion, const char *__file, int __line);


