/*
 * Tests of stub aliasing
 */


/* Aliases without parameters */
/* ************************** */

/*$
 * ensures: return == 0;
 */
int zero();

/*$
 * alias: zero;
 */
int _zero();

void test_alias_without_parameters() {
  _mopsa_assert(_zero() == 0);
}


/* Aliases with parameters */
/* *********************** */

/*$
 * ensures: return == x + 1;
 */
int incr(int x);

/*$
 * alias: incr;
 */
int _incr(int x);

void test_alias_with_parameter() {
  _mopsa_assert(_incr(1) == 2);
}
