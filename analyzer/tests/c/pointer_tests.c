#include "mopsa.h"
#include <stddef.h>

/*
 * Pointers to scalars
 */

void test_pointer_with_int_value() {
  int *p = (int*)1;
  _mopsa_assert_safe();
}

void test_deref_in_rval() {
  int i = 10;
  int* p = &i;
  i = i + 1;
  _mopsa_assert(*p == 11);
}

void test_deref_in_lval() {
  int i = 10;
  int* p = &i;
  *p = *p + 1;
  _mopsa_assert(i == 11);
}

void test_double_deref() {
  int i = 10;
  int* p = &i;
  int** q = &p;
  **q = *p + 1;
  _mopsa_assert(i == 11);
}

void test_equality() {
  int i;
  int *p, *q;
  p = &i;
  q = &i;
  _mopsa_assert(p == q);
}

void test_non_equality() {
  int i, j;
  int *p, *q;
  p = &i;
  q = &j;
  _mopsa_assert(p != q);
}

void test_null_is_zero() {
  int* p = NULL;
  _mopsa_assert(p == NULL);
}


void test_refine_top_pointer() {
  int *p = (int*)_mopsa_rand_pointer();
  if (p) _mopsa_assert(p != NULL);
}


int *global;

void test_initialization_to_null_of_uninitialized_global() {
  _mopsa_assert(global == NULL);
}

void test_deref_of_uninitialized_local_is_invalid() {
  int *p;
  int i = *p;
  _mopsa_assert_unsafe();
}

void test_null_deref() {
  int *p = NULL;
  int **q = &p;
  **q = 1;
  _mopsa_assert_unsafe();
}

void test_compare_pointers_with_int_values() {
  int *p = (int*)1;
  int *q = (int*)2;
  int *r = (int*)1;
  _mopsa_assert(p != q);
  _mopsa_assert(p == r);
}

/*
 * Pointers to arrays
 */

void test_array_deref() {
  int a[10];
  a[0] = 10;
  _mopsa_assert(*a == 10);
}

void test_multi_array_deref() {
  int a[5][5];
  a[0][0] = 10;
  _mopsa_assert(**a == 10);
}

void test_array_as_address() {
  int a[10];
  int *p;
  p = a;
  *p = 10;
  _mopsa_assert(a[0] == 10);
}

void test_multi_array_as_array_of_addresses() {
  int a[5][5];
  int *p;
  p = a[0];
  *p = 10;
  _mopsa_assert(a[0][0] == 10);
}

void test_address_of_multi_array() {
  int a[5][5];
  int *p;
  p = &a[0][0];
  *p = 20;
  _mopsa_assert(a[0][0] == 20);
}

void test_pointer_increment() {
  int a[10];
  int *p;
  p = a;
  p++;
  *p = 20;
  _mopsa_assert(a[1] == 20);
}

void test_pointer_decrement() {
  int a[10];
  int *p;
  p = a + 1;
  p--;
  *p = 20;
  _mopsa_assert(a[0] == 20);
}

void test_dereference_casted_pointer() {
  char a[10];
  a[0] = 0;
  a[1] = 0;
  ((int *) a)[1] = 1;
  _mopsa_assert(a[1] == 0);
}


/*
 * Pointers to records
 */

typedef struct {
  int x;
  int y;
} point;

void test_address_of_struct() {
  point a;
  point *p;
  p = &a;
  p->x = 10;
  _mopsa_assert(a.x == 10);
}

void test_pointer_increment_on_struct() {
  point a[10];
  point *p;
  p = a;
  p->x = 10;
  p++;
  p->x = 20;
  _mopsa_assert(a[0].x + a[1].x == 30);
}

void test_arrow_on_array_of_struct() {
  point a[10];
  a->x = 1;
  _mopsa_assert(a[0].x == 1);
}


/*
 * Pointers to functions
 */

int incr(int x) {
  return (x+1);
}

int ddouble(int x) {
  return 2 * x;
}

void test_call_function_pointer_by_deref() {
  int (*f)(int);
  f = incr;
  _mopsa_assert((*f)(0) == 1);
}

void test_call_function_pointer_as_function() {
  int (*f)(int);
  f = incr;
  _mopsa_assert(f(0) == 1);
}

void test_pointer_function_assignment() {
  int (*f)(int);
  void* g;
  g = (void*) ddouble;
  f = g;
  _mopsa_assert((f)(5) == 10);
}


/*
 * Pointer comparison
 */

void test_pointer_lt() {
  int a[10];
  int *p = a + 1;
  int *q = a + 2;
  _mopsa_assert(p < q);
}

void test_pointer_le() {
  int a[10];
  int *p = a + 1;
  int *q = a + 2;
  _mopsa_assert(p <= q);
}

void test_pointer_gt() {
  int a[10];
  int *p = a + 5;
  int *q = a + 2;
  _mopsa_assert(p > q);
}

void test_pointer_ge() {
  int a[10];
  int *p = a + 5;
  _mopsa_assert(p >= a);
}

void test_unsafe_pointer_le() {
  int i, j;
  int *p = &i;
  int *q = &j;
  int d = p <= q;
  _mopsa_assert_unsafe();
}



/*
 * Pointer difference
 */

void test_safe_pointer_difference_with_array() {
  int a[10];
  int *p = a + 1;
  _mopsa_assert(p - a == 1);
}

void test_safe_pointer_difference_with_pointer() {
  int a[10];
  int *p = a + 1;
  int *q = a + 3;
  _mopsa_assert(q - p == 2);
}

void test_unsafe_pointer_difference() {
  int i, j;
  int *p = &i;
  int *q = &j;
  int d = p - q;
  _mopsa_assert_unsafe();
}

void test_string2_1bptr_macro() {
  /* This macro tests that the object pointed by __x is one byte width
   * It is a workaround when __x is a void* pointer
   * Used in libc and defined in /usr/include/bits/string2.h:92
   */
  #define __string2_1bptr_p(__x) \
   ((size_t)(const void *)((__x) + 1) - (size_t)(const void *)(__x) == 1)

  int x;

  void *p1 =(void*)&x;
  _mopsa_assert(__string2_1bptr_p(p1));

  char *p2 = (char*)&x;
  _mopsa_assert(__string2_1bptr_p(p2));

  int *p3 = &x;
  _mopsa_assert(!__string2_1bptr_p(p3));
}
