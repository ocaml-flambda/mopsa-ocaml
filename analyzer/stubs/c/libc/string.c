#include <stddef.h>

/*@
 * requires: n <= size(s);
 *
 * assigns : s[0 .. n - 1];
 *
 * ensures : forall i in [0 .. n - 1]: s[i] == c
 *           and return == s;
 */
void *memset(void *s, int c, size_t n);

/*@
 * local: l1 = size(s1);
 * local: l2 = size(s2);
 *
 * predicate no_overlapping:
 *   (base(s1) == base(s2)) implies (s1 + n <= s2 or s2 + n <= s1);
 *
 * requires: n <= min(l1, l2)
 *           and no_overlapping;
 *
 * assigns: s1[0 .. n - 1];
 * ensures: forall i in [0 .. n - 1]: s1[i] == s2[i]
 *          and return == s1;
 */
void *memcpy (void *s1, const void *s2, size_t n);
