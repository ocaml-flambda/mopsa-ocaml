/*
 * Tests for variadic functions
 */

#include <stddef.h>
#include <stdarg.h>

char *_var = 0;
int _val = 0;

int env(char* var, ...) {
  if (var == NULL) {
    return _val;
  }
  _var = var;

  va_list ap;
  va_start(ap, var);
  int x = va_arg(ap, int);
  _val = x;
  va_end(ap);

  return _val;
}

void test_one_optional_argument() {
  _mopsa_assert(env(NULL) == 0);
  _mopsa_assert(env("a", 10) == 10);
  _mopsa_assert(env(NULL) == 10);
}
