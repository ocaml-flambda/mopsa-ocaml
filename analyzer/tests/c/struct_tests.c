#include "mopsa.h"

typedef struct {
  int x;
  int y;
} point;

typedef struct {
  point p1;
  point p2;
} segment;


void test_struct_field_in_lval() {
  point p;
  p.x = 1;
  p.y = 2;
  _mopsa_assert(p.x + p.y == 3);
}

void test_array_of_structs() {
  point a[5];
  a[0].x = 1;
  a[1].x = 2;
  _mopsa_assert(a[0].x + a[1].x == 3);
}

void test_initialization_with_expression_list() {
  point p = {1, 2};
  _mopsa_assert(p.x == p.y -1);
}

point global_point;
segment global_segment;


void test_initialization_uninitialized_global_struct() {
  _mopsa_assert(global_point.x == 0);
  _mopsa_assert(global_segment.p1.y == 0);
}

void test_initialization_with_designated_names() {
  point p = {.y = 2, .x = 1};
  _mopsa_assert(p.x == p.y -1);
}

point p3 = {.y = 2};

void test_partial_initialization_with_designated_names() {
  _mopsa_assert(p3.x == 0);
  _mopsa_assert(p3.y == 2);
}

void test_struct_copy() {
  point p = {.x = 1, .y = 2};
  point q = p;
  _mopsa_assert(p.x == q.x);
}

int f1(point p) {
  return p.x + 1;
}

int f2(point p) {
  p.x = p.x + 10;
  return p.x;
}

void test_pass_struct_by_value() {
  point p = {.x = 10, .y = 20};
  int x = f1(p);
  _mopsa_assert(x == 11);
  int y = f2(p);
  _mopsa_assert(p.x == 10 && y == 20);
}



typedef struct {
  int* f;
} s;

s a[2][2];

int buf[10];

void init(s* x) {
  x[1].f = buf;
}

void test_allamigeon() {
  s* ptr = (s*) &a[1];
  init(ptr);
  ptr = (s*) &a[0];
  a[1][1].f[2] = 10;
  _mopsa_assert_safe();
  a[1][1].f[10] = 20;
  _mopsa_assert_error(OUT_OF_BOUND);
}
