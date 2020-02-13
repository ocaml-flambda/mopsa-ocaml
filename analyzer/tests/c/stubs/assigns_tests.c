/* Test assignment on an integer scalar */
/* ************************************* */

/*$
 * assigns: *p;
 * ensures: (*p)' == v;
 */
void set(int*p, int v);

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
void set_to_a(char*p) ;

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
void incr(int*p, int s) ;

void test_assign_int_with_relation() {
  int x = 1;

  incr(&x, 10);
  _mopsa_assert(x == 11);
}


/* Test assignment on an pointer scalar */
/* ************************************ */

/*$
 * assigns: *ptr;
 * ensures: (*ptr)' == *ptr + s;
 */
void advance(int**ptr, int s) ;

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
void swap(int*p, int*q) ;

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
 * assigns: p[1, 1];
 * ensures: (p[1])' == n;
 */
void init_second(int *p, int n);

void test_assign_array() {
  int a[10] = {1, 2, 3};
  init_second(a, 5);
  _mopsa_assert(a[0] == 1);
  _mopsa_assert(a[1] == 5);
}



/*$
 * assigns: p[0, size(p) - 1];
 * ensures: (p[1])' == n;
 */
void partial_init_second(int *p, int n);

void test_assign_array_partially() {
  int a[10];
  a[0] = 0;
  a[1] = 1;
  partial_init_second(a, 5);
  _mopsa_assert_exists(a[0] == 0);
  _mopsa_assert_exists(a[0] == 100);
  _mopsa_assert(a[1] == 5);
}


/*$
 * assigns: p[0,9];
 */
void rand_array(int p[10]);

void test_assign_array_full() {
  int a[10];
  a[0] = 1;
  a[1] = 1;
  a[9] = 1;
  rand_array(a);
  _mopsa_assert_exists(a[0] == 0);
  _mopsa_assert_exists(a[0] == 100);
  _mopsa_assert_exists(a[9] == 200);
}


/* Assignments of structs */
/* ********************** */

struct s {
  int n;
  int *p;
  int a[10];
};

/*$
 * assigns: *p;
 */
void rand_struct(struct s *p);

void test_assign_struct() {
  struct s x;
  int v;
  x.n = 10;
  x.p = &v;
  x.a[0] = 5;
  rand_struct(&x);
  _mopsa_assert_exists(x.n != 10);
  _mopsa_assert_exists(x.p != &v);
  _mopsa_assert_exists(x.a[0] != 5);
}
  
  


/* Assign global variables */
/* *********************** */
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
