/*
 * Tests for conditional expressions `cond?e1:e2`
 */

#include "mopsa.h"

void test_one_condition() {
  int x = 1;
  int y = (x >= 1) ? 0 : 1;
  _mopsa_assert(y == 0);
}

#define MAP(_)        \
  ((_) == 'A' ? 0     \
   : (_) == 'B' ? 1   \
   : (_) == 'C' ? 2   \
   : (_) == 'D' ? 3   \
   : -1)

void test_chain_of_conditions() {
  int a = MAP('A');
  _mopsa_assert(a == 0);

  int b = MAP('B');
  _mopsa_assert(b == 1);

  int z = MAP('Z');
  _mopsa_assert(z == -1);
}

void test_nondet_cond() {
  int a = _mopsa_rand_int();
  int b = (a == 0) ? 100 : -100;
  _mopsa_assert_exists(b == 100);
  _mopsa_assert_exists(b == -100);
}

int x;

int f() {
  x = 1;
  return ((x++ == 1) && x == 2) ? 1 : 0;
}

void test_sideeffect() {
  int r = f();
  _mopsa_assert(x == 2);
  _mopsa_assert(r == 1);
}
