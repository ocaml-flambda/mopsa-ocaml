#include <string.h>

int main(int argc, char *argv[]) {
  char a[10];
  memset(a, 0, 10);
  int l = strlen(a);
  _mopsa_print();
  return 0;
}
