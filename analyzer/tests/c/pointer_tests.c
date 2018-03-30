#include "mopsa.h"

void test_deref_in_rval() {
  int i = 10;
  int* p = &i;
  i = i + 1;
  _mopsa_assert_true(*p == 11);
}

void test_deref_in_lval() {
  int i = 10;
  int* p = &i;
  *p = *p + 1;
  _mopsa_assert_true(i == 11);
}

void test_double_deref() {
  int i = 10;
  int* p = &i;
  int** q = &p;
  **q = *p + 1;
  _mopsa_assert_true(i == 11);
}

void test_equality() {
  int i;
  int *p, *q;
  p = &i;
  q = &i;
  _mopsa_assert_true(p == q);
}

void test_non_equality() {
  int i, j;
  int *p, *q;
  p = &i;
  q = &j;
  _mopsa_assert_true(p != q);
}

void test_array_deref() {
  int a[10];
  a[0] = 10;
  _mopsa_assert_true(*a == 10);
}

void test_multi_array_deref() {
  int a[10][10];
  a[0][0] = 10;
  _mopsa_assert_true(**a == 10);
}

void test_array_as_address() {
  int a[10];
  int *p;
  p = a;
  *p = 10;
  _mopsa_assert_true(a[0] == 10);
}

void test_multi_array_as_array_of_addresses() {
  int a[10][10];
  int *p;
  p = a[0];
  *p = 10;
  _mopsa_assert_true(a[0][0] == 10);
}

void test_address_of_array() {
  int a[10];
  int *p;
  p = &a;
  *p = 10;
  _mopsa_assert_true(a[0] == 10);
}

void test_address_of_multi_array() {
  int a[10][10];
  int *p, *q;
  p = &a[0];
  *p = 10;
  _mopsa_assert_true(a[0][0] == 10);
  q = &a[0][0];
  *q = 20;
  _mopsa_assert_true(a[0][0] == 20);
}

void test_pointer_increment() {
  int a[10];
  int *p;
  p = &a;
  p++;
  *p = 20;
  _mopsa_assert_true(a[1] == 20);
}

typedef struct {
  int x;
  int y;
} point;

void test_address_of_struct() {
  point a;
  point *p;
  p = &a;
  p->x = 10;
  _mopsa_assert_true(a.x == 10);
}

void test_pointer_increment_on_struct() {
  point a[10];
  point *p;
  p = &a;
  p->x = 10;
  p++;
  p->x = 20;
  _mopsa_assert_true(a[0].x + a[1].x == 30);
}

void test_arrow_on_array_of_struct() {
  point a[10];
  a->x = 1;
  _mopsa_assert_true(a[0].x == 1);
}
