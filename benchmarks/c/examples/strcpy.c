#include <stdio.h>
#include <string.h>

int main() {
  char s1[20];
  char s2[40] = "toto";
  strcpy(s1,s2);
  printf("s1 = %s\n", s1);
}
