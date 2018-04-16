#include <stddef.h>
#include "mopsa.h"

void * malloc(size_t size) {
  _mopsa_panic("malloc not supported");
  return NULL;
}
