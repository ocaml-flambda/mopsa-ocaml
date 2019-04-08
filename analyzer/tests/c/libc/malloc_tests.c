/*
 * Tests of malloc allocation function
 */

#include <stdlib.h>
#include <limits.h>

/* Utility function */
void *safe_malloc(size_t n) {
  void *p = malloc(n);
  if (p == NULL) abort();
  return p;
}

/********************/
/* Tests on scalars */
/********************/

void test_malloc_is_non_deterministic() {
  int *p = (int *)malloc(sizeof(int));
  _mopsa_assert_exists(p == NULL);
  _mopsa_assert_exists(p != NULL);
}

void test_malloc_returns_writable_memory() {
  int *p = (int *)malloc(sizeof(int));
  if (p != NULL) {
    *p = 10;
    _mopsa_assert(*p == 10);
  }
}

void test_malloc_allocates_required_size() {
  int *n = (int *)safe_malloc(sizeof(int));
  *n = INT_MAX;
  _mopsa_assert_safe();
  _mopsa_assert(*n == INT_MAX);
}

void test_malloc_pointer() {
  int n = 10;
  int **p = (int **)safe_malloc(sizeof(int*));
  *p = &n;
  _mopsa_assert_safe();
  _mopsa_assert(**p == n);
}

/*******************/
/* Tests on arrays */
/*******************/

void test_malloc_array() {
  int *a = (int *)safe_malloc(10 * sizeof(int));
  a[10] = 10;
  _mopsa_assert_unsafe();
}

void test_malloc_array_arithmetics() {
  int *a = (int *)safe_malloc(10 * sizeof(int));
  *a = 0;
  a++;
  *a = 1;
  a[8] = 9;
  _mopsa_assert_safe();
  _mopsa_assert(a[-1] == a[8] - 9);
}

/*********************************/
/* Tests of allocations in loops */
/*********************************/

void test_malloc_in_loop() {
  int n = 0;
  int *cur = &n, *prev;
  for(int i = 0; i < 100; i++) {
    prev = cur;
    cur = (int*)safe_malloc(sizeof(int));
    *cur = 1;
  }
  _mopsa_assert(cur != &n);
  _mopsa_assert_exists(prev != &n);

  *cur = *cur + 1;
  _mopsa_assert(*cur == 2);
  _mopsa_assert(n == 0);
  _mopsa_assert_exists(*prev == 1);

  *prev = *prev + 1;
  _mopsa_assert_exists(n == 0);
  _mopsa_assert_exists(n == 1);
}
