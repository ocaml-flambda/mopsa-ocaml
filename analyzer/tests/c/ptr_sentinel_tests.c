#include <assert.h>
#include <string.h>

int main(int g, char **d) {
  *d = "";
  char *b = *d;
  assert(strlen(b) == 0);
}

