#include <stdio.h>

char * _strcpy(char *dst, const char *src)
{
  while ((*dst++ = *src++) != 0)
    ;
  return dst;
}

int main() {
  char s1[20];
  char s2[40] = "toto";
  _strcpy(s1,s2);
  printf("s1 = %s\n", s1);
}
