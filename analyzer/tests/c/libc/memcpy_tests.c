/*
 * Tests of the memcpy function
 */

#include <stdlib.h>
#include <string.h>

void test_safe_memcpy_on_stack_arrays() {
  char a[5] = {1,2,3,4,5};
  char b[5];
  memcpy(b,a,5);
  _mopsa_assert_safe();
}

void test_unsafe_memcpy_on_overlapping_stack_arrays() {
  char a[5] = {1,2,3,4,5};
  memcpy(a+1,a,2);
  _mopsa_assert_unsafe();
}

void test_safe_memcpy_on_malloc_arrays() {
  char *a = (char*)malloc(5);
  char *b = (char*)malloc(5);
  if (a && b) {
    memcpy(b,a,5);
    _mopsa_assert_safe();
  }
}

void test_unsafe_memcpy_on_overlapping_malloc_arrays() {
  char *a = (char*)malloc(5);
  if (a) {
    memcpy(a+1,a,2);
    _mopsa_assert_unsafe();
  }
}
