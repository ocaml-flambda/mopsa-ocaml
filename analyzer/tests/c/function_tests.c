#include "mopsa.h"

/* Test of scalar arguments */
/* ************************ */

int incr(int x) {
  return x + 1;
}

int mult(int x, int y) {
  return x * y;
}

void test_scalar_argument() {
  int i;
  i = 10;
  _mopsa_assert(incr(i) == 11);
}


/* Test of array arguments */
/* *********************** */

typedef int int_array[1];

void reset_first(int_array a) {
  a[0] = 0;
}

void test_array_argument() {
  int_array arr = {1};
  reset_first(arr);
  _mopsa_assert(arr[0] == 0);
}


/* Test of struct arguments */
/* ************************ */

typedef struct {
  int x;
  int y;
} point;

int read_struct_member(point p) {
  return p.x + 1;
}

int change_struct_member(point p) {
  p.x = p.x + 10;
  return p.x;
}

void test_struct_argument() {
  point p = {.x = 10, .y = 20};
  int x = read_struct_member(p);
  _mopsa_assert(x == 11);
  int y = change_struct_member(p);
  _mopsa_assert(p.x == 10 && y == 20);
}

/* Test of struct array arguments */
/* ****************************** */

typedef struct {
  int x;
} struct_array[1];

void reset_struct_array(struct_array a) {
  a[0].x = 0;
}

void test_struct_array_argument() {
  struct_array arr;
  reset_struct_array(arr);
  _mopsa_assert(arr[0].x == 0);
}


/* Test of procedures */
/* ****************** */

int proc_g;

void proc(int x) {
  proc_g = x;
}

void test_procedure() {
  proc(10);
  _mopsa_assert(proc_g == 10);
}



/* Test of nested calls */
/* ******************** */

int max(int x, int y) {
  int res;
  if (x > y)
    res = x;
  else
    res = y;
  return res;
}

void test_nested_call() {
  _mopsa_assert(max(1, max(2, 3)) == 3);
}


/* Test of a call with a previous return */
/* ************************************** */

int ff() {
  if (_mopsa_rand_int()) {
    return 1;
  }
  int x = max(2,3);
  return x;
}

void test_call_with_previous_return() {
  int a;
  a = ff();
  _mopsa_assert_exists(a == 1);
  _mopsa_assert_exists(a == 3);
}
