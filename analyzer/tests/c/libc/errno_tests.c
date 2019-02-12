/*
 * Tests for functions in errno.h
 */

#include <errno.h>
#include <stddef.h>

void test_errno_location_returns_non_null() {
  _mopsa_assert(__errno_location() != NULL);
}
