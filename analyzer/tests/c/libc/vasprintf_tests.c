#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>

void test_constant_strings() {
  char *i1 = "blabliblu";
  char *i2 = "";
  char *r1;
  char *r2;
  va_list p;
  int l1 = vasprintf(&r1, i1, p);
  int l2 = vasprintf(&r2, i2, p);
  _mopsa_assert(l1 == strlen(r1));
  _mopsa_assert(l2 == strlen(r2));
  _mopsa_assert_safe();
}
