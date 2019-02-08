/* Test assignment on an integer scalar */
/* ************************************* */

/*$
 * assigns: *p;
 * ensures: (*p)' == v;
 */
void set(int*p, int v) {}

void test_assign_int() {
  int x = 1;

  set(&x, 10);
  _mopsa_assert(x == 10);
}

/* Test assignment on an char */
/* ************************** */

/*$
 * assigns: *p;
 * ensures: (*p)' == 'a';
 */
void set_to_a(char*p) {}

void test_assign_char() {
  char x;
  set_to_a(&x);
  _mopsa_assert(x == 'a');
}


/* Test assignment on an integer scalar with a relation */
/* **************************************************** */

/*$
 * assigns: *p;
 * ensures: (*p)' == *p + s;
 */
void incr(int*p, int s) {}

void test_assign_int_with_relation() {
  int x = 1;

  incr(&x, 10);
  _mopsa_assert(x == 11);
}


/* Test assignment on an pointer scalar */
/* ************************************ */

/*$
 * assigns: *p;
 * ensures: (*p)' == *p + s;
 */
void advance(int**p, int s) {}

void test_assign_ptr() {
  int a [5] = {0, 1, 2, 3, 4};
  int *p = a;

  advance(&p, 1);
  _mopsa_assert(p == &(a[1]));
}


/* Test assignments on two scalars */
/* ******************************* */

/*$
 * assigns: *p;
 * assigns: *q;
 * ensures: (*p)' == *q and (*q)' == *p;
 */
void swap(int*p, int*q) {}

void test_assign_two_scalars() {
  int x = 1;
  int y = 2;

  swap(&x, &y);
  _mopsa_assert(x == 2);
  _mopsa_assert(y == 1);
}

/* Assignments on arrays */
/* ********************* */

/*$
 * assigns: a[1, 1];
 * ensures: (a[1])' == n;
 */
void init_second(int *a, int n);

void test_assign_array() {
  int a[10] = {1, 2, 3};
  init_second(a, 5);
  _mopsa_assert(a[0] == 1);
  _mopsa_assert(a[1] == 5);
}

/* Assign global variables */

int glob = 1;

/*$
 * assigns: glob;
 * ensures: glob' == n;
 */
void change_global(int n);

void test_assign_global() {
  _mopsa_assert(glob == 1);
  change_global(10);
  _mopsa_assert(glob == 10);
}
