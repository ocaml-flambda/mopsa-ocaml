/* ************************ */
/* Tests of stub directives */
/* ************************ */


/* ************************ */
/* Modification of integers */
/* ************************ */

int x;

/*$!
 * assigns: x;
 * ensures: x' == 10;
 */

void test_init_integer() {
  _mopsa_assert(x == 10);
}


/* ************************ */
/* Modification of pointers */
/* ************************ */

int *p;

/*$!
 * assigns: p;
 * ensures: p' == &x;
 */

void test_init_pointer() {
  _mopsa_assert(p == &x);
}


/* ********************** */
/* Modification of arrays */
/* ********************** */

int a[10];

/*$!
 * assigns: a[0,9];
 * ensures: a[0]' == 20;
 */

void test_init_array() {
  _mopsa_assert(a[0] == 20);
}
