#include <mopsa.h>

int error;

int f(char* buf, int size, double x) {
  _mopsa_assert(size > 0);
  _mopsa_assert_valid_bytes(buf, size);
  _mopsa_assert(__builtin_isfinite(x));

  if (_mopsa_rand_s8()) {
    int r;
    _mopsa_assume(r >= 0 && r <= size);
    if (r > 0) _mopsa_memrand(buf, 0, r-1);
    _mopsa_memset(buf, 0, r, size-1);
    return r;
  }
  else {
    error = 1;
    return -1;
  }
}

void main() {
  char buf[256];
  f(buf, 256, 12.);
}
