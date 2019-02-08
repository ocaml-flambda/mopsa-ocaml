#include <string.h>

int main(int argc, char *argv[]) {
  int a[3];
  int b[3] = {1, 2, 3};
  memcpy(a, b, 3);
  return 0;
}
