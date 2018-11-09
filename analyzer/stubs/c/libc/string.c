#include <stddef.h>

/*@
requires: n <= size(s);
assigns : s[0 .. n - 1];
ensures : forall i in [0 .. n - 1]: s[i] == c
          and return == s;
*/
void *memset(void *s, int c, size_t n) { }
