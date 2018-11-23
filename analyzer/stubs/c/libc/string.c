#include <stddef.h>

/*$
 * requires: n <= size(s);
 * assigns : ((char*)s)[0 .. n - 1];
 * ensures : forall int i in [0 .. n - 1]: ((char*)s)[i] == c
 *           and return == s;
 */
void *memset(void *s, int c, size_t n) { }
