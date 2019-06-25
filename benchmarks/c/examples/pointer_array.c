#include <stdio.h>

int main() {
  char *a[] = { "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", NULL, "o", "p"};
  char **p = a;
  while(*p) p++;
  printf("%c\n", *p[0]);
}
