extern int __VERIFIER_nondet_int(void);

int main() {
  int a = __VERIFIER_nondet_int();
  _mopsa_assume(a <= 1);
  double x = a / 2.0;
  int cond = 2 * x >= 0.0;
  _mopsa_assert_safe();
  _mopsa_assert(x <= 0.0);
  _mopsa_assert_unsafe();
  return 0;
}
