/*
 * Tests for variadic functions
 */

#include <string.h>
#include <stddef.h>
#include <stdarg.h>

/*********************************************************/
/* Test of variadic functions with one optional argument */
/*********************************************************/

int _last = 0;

/* Dummy addition function. When called with a valid pointer to an
   integer, it adds its two arguments and stores the result in the
   first argument. If the first argument is NULL, it returns the
   result of the last computation */
int add(int* p, ...) {
  if (p == NULL) {
    return _last;
  }
  va_list ap;
  va_start(ap, p);

  int x = va_arg(ap, int);
  *p = *p + x;
  _last = *p;

  va_end(ap);
  return _last;
}

void test_one_optional_argument() {
  int x = 1;
  _mopsa_assert(add(NULL) == 0);
  _mopsa_assert(add(&x, 10) == 11);
  _mopsa_assert(x == 11);
  _mopsa_assert(add(NULL) == 11);
}


/************************************************************/
/* Test of passing a va_list argument to variadic functions */
/************************************************************/

int _last2 = 0;

int vadd(int *p, va_list app) {
  if (p == NULL) {
    return _last2;
  }
  int x = va_arg(app, int);
  *p = *p + x;
  _last2 = *p;
  return _last2;
}

int add_abs(int *p, ...) {
  va_list ap;
  va_start(ap, p);
  int ret = vadd(p, ap);
  if (ret < 0) {
    ret = -ret;
    _last2 = ret;
  }
  va_end(ap);
  return ret;
}

void test_pass_va_list_to_variadic_function() {
  int x = 1;
  _mopsa_assert(add_abs(NULL) == 0);
  _mopsa_assert(add_abs(&x, 1) == 2);
  _mopsa_assert(add_abs(NULL) == 2);
  _mopsa_assert(add_abs(&x, -10) == 8);
}




/*************************************************/
/* Test of cascading calls to variadic functions */
/*************************************************/

int f3(int x, ...) {
  va_list ap;
  va_start(ap, x);

  int y = va_arg(ap, int);

  va_end(ap);
  return x * y;
}


int f2(int x, va_list ap) {
  int z = va_arg(ap, int);
  return f3(x, x + z);
}

int f1(int x, ...) {
  va_list ap;
  va_start(ap, x);
  int y = f2(x, ap);
  va_end(ap);
  return y;
}


void test_cascading_calls_to_variadic_functions() {
  _mopsa_assert(f1(10, 20) == 300);
}

/*******************/
/* Test of va_copy */
/*******************/

int g1(int x, ...) {
  va_list ap1, ap2;
  va_start(ap1, x);
  int y = va_arg(ap1, int);
  va_copy(ap2, ap1);
  va_end(ap1);
  int z = va_arg(ap2, int);
  va_end(ap2);
  return x + y + z;
}

void test_va_copy() {
  _mopsa_assert(g1(1,2,3) == 6);
}


/********************/
/* Test broken cast */
/********************/

struct bla {};

size_t f(int d, ...) {
  va_list l;
  va_start(l, d);
  char *x = va_arg(l, char *);
  va_end(l);
  return strlen(x);
}

void test_cast() {
  _mopsa_assert(f(1, "abc") == 3);
  _mopsa_assert_safe();
  f(1, 1);
  _mopsa_assert_unsafe();
}

void test_cast2() {
  struct bla s;
  f(1, s);
  _mopsa_assert_unsafe();
}
