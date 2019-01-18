/*
 * Tests of functions for dynamic memory allocation
 */

#include <stdlib.h>
#include <limits.h>

void test_malloc_of_int() {
  int *n = (int *)malloc(sizeof(int));
  _mopsa_print();
  _mopsa_assert_exists(n == NULL);
  if (n != NULL) {
    *n = 10;
    _mopsa_assert(*n == 10);
  }
}


void *safe_malloc(size_t n) {
  void *p = malloc(n);
  if (p == NULL)
    abort();
  return p;
}

void test_safe_malloc_of_int() {
  int *n = (int *)safe_malloc(sizeof(int));
  *n = INT_MAX;
  _mopsa_assert_safe();
  _mopsa_assert(*n == INT_MAX);
}

void test_safe_malloc_of_ptr() {
  int **p = (int **)safe_malloc(sizeof(int*));
  int n = 10;
  *p = &n;
  _mopsa_assert_safe();
  _mopsa_assert(**p == n);
}

void test_safe_malloc_of_array() {
  int *a = (int *)safe_malloc(10 * sizeof(int));
  a[0] = 0;
  a[1] = 1;
  a[9] = 9;
  _mopsa_assert_safe();
  _mopsa_assert(a[0] == a[9] - 9);
  a[10] = 10;
  _mopsa_assert_unsafe();
}
