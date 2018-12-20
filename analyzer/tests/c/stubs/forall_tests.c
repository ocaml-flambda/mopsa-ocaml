/* Test the modification of part of an array */
/* ***************************************** */

/*$
 * requires: l in [0, size(p) - 1] and
 *           u in [0, size(p) - 1];
 * assigns: p[l, u];
 * ensures: forall int i in [l, u]: (p[i])` == 0;
 */
void reset_region(int*p, int l, int u) {}

void test_set_part() {
  int a[10];
  a[0] = 100;
  a[3] = 100;
  a[5] = 100;
  reset_region(a, 3, 9);
  _mopsa_assert(a[0] == 100);
  _mopsa_assert(a[3] == 0);
}


/* Test the modification of an entire array with a constant */
/* ******************************************************** */

/*$
 * requires: n in [0, size(p)];
 * assigns: ((char*)p)[0, n - 1];
 * ensures: forall int i in [0, n - 1]: (((char*) p)[i])` == c;
 */
void set_all(void*p, char c, unsigned int n) {}

void test_set_all() {
  char a[5] = {0, 1, 2, 3, 4};

  set_all(a, 10, 5);
  _mopsa_assert(a[0] == 10);
}


/* Test the modification of a multi-dimensional array with a constant */
/* ****************************************************************** */

/*$
 * requires: m in [0, size(p)] and
 *           n <= 5;
 * assigns: p[0, m - 1][0, n - 1];
 * ensures: forall int i in [0, m - 1]:
 *            forall int j in [0, n - 1]:
 *              (p[i][j])` == c;
 */
void set_all2(char p[][5], char c, unsigned int m, unsigned int n);

void test_set_all_multi_dim() {
  char a[5][5] = {{1, 2, 3, 4}};

  set_all2(a, 10, 5, 5);
  _mopsa_assert(a[0][0] == 10);
}


/* Test dependencies in embedded quantifications on multi-dimensional arrays */
/* ************************************************************************* */

/*$
 * assigns: p[0, 3][0, 3];
 * ensures: forall int i in [0, 3]:
 *            forall int j in [0, i]:
 *              (p[i][j])` == c;
 */
void triangle_init(char p[][4], char c);

void test_dependent_forall() {
  char a[4][4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}, {13, 14, 15, 16}};

  triangle_init(a, 100);
  _mopsa_assert(a[0][0] == 100);
  // _mopsa_assert(a[0][1] == 2); // FIXME: this assertion can not be verified
                                  // since the `assigns` section can not express
                                  // dependencies between modified cells
}
