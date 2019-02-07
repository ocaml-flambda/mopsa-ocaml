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
#include <errno.h>
#include "mopsa_libc_utils.h"

int _errno;

/*$
 * ensures: return == &_errno;
 */
int *__errno_location (void);

#ifdef __USE_GNU

/*$
 * local: char* addr = ReadOnlyString;
 * ensures: size(addr) > 1;
 * ensures: valid_string(addr);
 * ensures: program_invocation_name == addr;
 */
const char *program_invocation_name;

/*$
 * local: char* addr = ReadOnlyString;
 * ensures: size(addr) > 1;
 * ensures: valid_string(addr);
 * ensures: program_invocation_short_name;
 */
const char *program_invocation_short_name;

#endif
