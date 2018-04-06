#include "mopsa.h"

typedef struct {
  int x;
  int y;
} point;

point global_point;

typedef struct {
  point p1;
  point p2;
} segment;

segment global_segment;

void test_struct_field_in_lval() {
  point p;
  p.x = 1;
  p.y = 2;
  _mopsa_assert_true(p.x + p.y == 3);
}

void test_array_of_structs() {
  point a[5];
  a[0].x = 1;
  a[1].x = 2;
  _mopsa_assert_true(a[0].x + a[1].x == 3);
}

void test_initialization_with_expression_list() {
  point p = {1, 2};
  _mopsa_assert_true(p.x == p.y -1);
}

void test_initialization_uninitialized_global_struct() {
  _mopsa_assert_true(global_point.x == 0);
  _mopsa_assert_true(global_segment.p1.y == 0);
}

void test_initialization_with_designated_names() {
  point p = {.y = 2, .x = 1};
  _mopsa_assert_true(p.x == p.y -1);
}
