#include <stdio.h>
#include <string.h>

void main() {
  char s[101];
  unsigned int n = _mopsa_range_u32(1, 100);
  memset(s, 'a', n);
  s[n] = '\0';
  printf("len(%s) = %d\n", s, strlen(s));
}
