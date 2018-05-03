#include<stdio.h>
#include "mopsa.h"
#include "stddef.h"

char* fgets(char *str, int n, FILE *stream) {
  for (int i = 0; i < n ; i++ ) {
    str[i] = _mopsa_range_char();
  };
  if (_mopsa_rand_int(0,1)) {
    return str;
  } else {
    return NULL;
  }
}

int atoi(const char *str) {
  return _mopsa_range_int();
}
