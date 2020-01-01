#include <string.h>

int main(int argc, char *argv[]) {
  char a[10] = "toto";
  char b[10];
  memcpy(b, a, 10);
  _mopsa_print();
  return 0;
}
