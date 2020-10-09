#include <stddef.h>
#include <math.h>

/* Tests for sizeof built-in */
/* ************************* */

/*$
 * ensures: return == sizeof_expr(x);
 */
int sizeof_int(int x);

void test_sizeof_int() {
  int x;
  int s = sizeof_int(x);
  _mopsa_assert(s == sizeof(x));
}


struct point {
  int x;
  int y;
};

/*$
 * ensures: return == sizeof_expr(*x);
 */
int sizeof_struct_expr(struct point *x);

void test_sizeof_struct_expr() {
  struct point p;
  int s = sizeof_struct_expr(&p);
  _mopsa_assert(s == sizeof(p));
}

/*$
 * ensures: return == sizeof_type(struct point);
 */
int sizeof_struct_type(struct point *x);

void test_sizeof_struct_type() {
  struct point p;
  int s = sizeof_struct_type(&p);
  _mopsa_assert(s == sizeof(struct point));
}

typedef struct point point_t;

/*$
 * ensures: return == sizeof_type(point_t);
 */
int sizeof_struct_typedef(point_t *x);

void test_sizeof_struct_typedef() {
  point_t p;
  int s = sizeof_struct_typedef(&p);
  _mopsa_assert(s == sizeof(point_t));
}




/* Tests for valid_ptr built-in*/
/* **************************** */

/*$
 * requires: valid_ptr(p);
 */
void check_int_ptr(int *p);

void test_valid_ptr_on_int() {
  int x;
  int *p = &x;
  check_int_ptr(p);
  _mopsa_assert_safe();
}

void test_valid_ptr_on_null() {
  int *p = NULL;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}

void test_valid_ptr_on_invalid() {
  int *p;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}

void test_valid_ptr_on_offset_overflow() {
  int a[10];
  int *p = a;
  p = p + 10;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}

/*  Tests for valid_float built-in */
/*  ****************************** */

/*$
 * requires: valid_float(f);
 */
void check_float(float f);

void test_valid_float_on_initialized_float() {
  float f = 2.3;
  check_float(f);
  _mopsa_assert_safe();
}

void test_valid_float_on_nan() {
  float f = NAN;
  check_float(f);
  _mopsa_assert_unsafe();
}

void test_valid_float_on_infinity() {
  float f = INFINITY;
  check_float(f);
  _mopsa_assert_unsafe();
}
