/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/

/*
 * Tests for functions in errno.h
 */

#include <errno.h>
#include <stddef.h>

void test_errno_location_returns_non_null() {
  _mopsa_assert(__errno_location() != NULL);
}
