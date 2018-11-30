#include "mopsa.h"

/* Test assignment on an integer scalar */
/* ************************************* */

/*$
 * assigns: *p;
 * ensures: *p == v;
 */
void set(int*p, int v) {}

void test_assign_int() {
  int x = 1;

  set(&x, 10);
  _mopsa_assert(x == 10);
}


/* Test assignment on an integer scalar with a relation */
/* **************************************************** */

/*$
 * assigns: *p;
 * ensures: *p == old(*p) + s;
 */
void incr(int*p, int s) {}

void test_assign_int_with_old() {
  int x = 1;

  incr(&x, 10);
  _mopsa_assert(x == 11);
}


/* Test assignment on an pointer scalar */
/* ************************************ */

/*$
 * assigns: *p;
 * ensures: *p == old(*p) + s;
 */
void advance(int**p, int s) {}

void test_assign_ptr_with_old() {
  int a [5] = {0, 1, 2, 3, 4};
  int *p = a;

  advance(&p, 1);
  _mopsa_assert(p == &(a[1]));
}


/* Test assignments on two scalars */
/* ******************************* */

/*$
 * requires: p != q;
 * assigns: *p;
 * assigns: *q;
 * ensures: *p == old(*q) and *q == old(*p);
 */
void swap(int*p, int*q) {}

void test_assign_two_scalars() {
  int x = 1;
  int y = 2;

  swap(&x, &y);
  _mopsa_assert(x == 2);
  _mopsa_assert(y == 1);
}
