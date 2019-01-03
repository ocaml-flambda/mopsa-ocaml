#include <stddef.h>

/* Test user defined macros */
/* ************************ */

#define TEN 10
#define TEN_PLUS_TWO (TEN + 2)

/*$
 * ensures: return == TEN;
 */
int ten();

/*$
 * ensures: return == TEN_PLUS_TWO;
 */
int ten_plus_two();

void test_constant_macro() {
  int n = ten();
  _mopsa_assert(n == TEN);
}


void test_expression_macro() {
  int n = ten_plus_two();
  _mopsa_assert(n == TEN_PLUS_TWO);
}


/* Test standard library macros */
/* **************************** */

/*$
 * requires: ptr != NULL;
 * ensures : return == *ptr;
 */
int deref(int *ptr);

void test_null_macro() {
  int x = 100;
  int *p = &x;
  int y = deref(p);
  _mopsa_assert_safe();
  _mopsa_assert(y == x);
}

