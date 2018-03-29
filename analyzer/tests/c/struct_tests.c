#include "mopsa.h"

typedef struct {
  int x;
  int y;
} point;

void test_struct_field_in_lval() {
  point p;
  p.x = 1;
  p.y = 2;
  _mopsa_assert_true(p.x == 1);
}
