// mopsa-c -config=c/cell-string-length-pack-rel-itv.json -c-pack-resources -stub-ignore-case=malloc.failure
#include <stdlib.h>
#include <assert.h>

int main() {
  unsigned int n1 = _mopsa_range_u32(1, 100);
  unsigned int n2 = _mopsa_range_u32(1, 100);
  char *s1 = malloc((n1 + 1) * sizeof(char));
  s1[n1] = 0;
  /** Fixed regression:
      the merge operator from the relational domain introduced an imprecision here, with a constraint `strlen(s1) >= -1`
      We can't check it directly with an assert, because the reduction between intervals and relational would have been triggered. However, the issue triggers an imprecision on the last statement of this program
   */
  char *s2 = malloc((n1 + n2 + 1) * sizeof(char));
  s2[n2] = 0;
}
