#include <stdio.h>

void init(char *arr[], int n) {
  int i = 0;
  while (i < n) {
    arr[i] = "toto";
    i++;
  }
  arr[i] = NULL;
}

int main() {
  char *a[100];
  init(a, 20);

  char **p = a;
  while(*p) p++;
  p--;
  printf("%c\n", *p[0]);
}
