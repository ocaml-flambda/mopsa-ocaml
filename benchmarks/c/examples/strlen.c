#include <string.h>


void main() {
  char s[101];
  unsigned int n = _mopsa_range_u32(1, 100), i;
  for(i = 0; i < n; i++) {
    s[i] = 'a';
  }
  s[i] = '\0';
  int l = strlen(s);
  _mopsa_print();
}
