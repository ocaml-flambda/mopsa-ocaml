#include "mopsa.h"

typedef struct {
  int x;
  int y;
} point;

void test_struct_field_in_lval() {
  point p;
  p.x = 1;
  p.y = 2;
  _mopsa_assert_true(p.x + p.y == 3);
}

void test_array_of_structs() {
  point a[10];
  a[0].x = 1;
  a[1].x = 2;
  _mopsa_assert_true(a[0].x + a[1].x == 3);
}

void test_initialization_with_compound_litteral() {
  point p = {1, 2};
  _mopsa_assert_true(p.x == p.y -1);
}
