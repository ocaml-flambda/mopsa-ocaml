/* Test calling sizeof built-in on an integer */
/* ****************************************** */

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


/* Test calling sizeof built-in on an structure */
/* ****************************************** */

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
