/*
 * Tests of strlen
 */

#include <string.h>

void test_strlen_on_empty_string() {
  char* s = "";
  size_t n = strlen(s);
  _mopsa_assert(n == 0);
}
