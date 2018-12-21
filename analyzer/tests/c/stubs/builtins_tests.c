#include <stddef.h>

/* Tests for sizeof built-in */
/* ************************* */

/*$
 * ensures: return == sizeof(x);
 */
int sizeof_int(int x);

void test_sizeof_int() {
  int x;
  int s = sizeof_int(x);
  _mopsa_print();
  _mopsa_assert(s == sizeof(x));
}


typedef struct {
  int x;
  int y;
} point;

/*$
 * ensures: return == sizeof(*x);
 */
int sizeof_struct(point *x);

void test_sizeof_struct() {
  point p;
  int s = sizeof_struct(&p);
  _mopsa_print();
  _mopsa_assert(s == sizeof(p));
}




/* Tests for ptr_valid built-in*/
/* **************************** */

/*$
 * requires: ptr_valid(p);
 */
void check_int_ptr(int *p);

void test_ptr_valid_on_int() {
  int x;
  int *p = &x;
  check_int_ptr(p);
  _mopsa_assert_safe();
}

void test_ptr_valid_on_null() {
  int *p = NULL;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}

void test_ptr_valid_on_invalid() {
  int *p;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}

void test_ptr_valid_on_offset_overflow() {
  int a[10];
  int *p = a;
  p = p + 10;
  check_int_ptr(p);
  _mopsa_assert_unsafe();
}
