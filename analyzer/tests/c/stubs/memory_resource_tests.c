#include <stdlib.h>

/*$
 * local   : void *addr = new Memory;
 * ensures : size(addr) == n and
 *           return == addr;
 */
void *malloc_(size_t n);

void test_new_int() {
  int *x = (int*)malloc_(sizeof(int));
  *x = 10;
  _mopsa_assert(*x == 10);
}

void test_new_int_array() {
  int *a = (int*)malloc_(10 * sizeof(int));
  a[0] = 10;
  a[9] = 100;
  _mopsa_assert(a[0] + a[9] == 110);
  int x = a[10];
  _mopsa_assert_unsafe();
}



/*$
 * requires: p in Memory;
 * free: p;
 */
void free_(void*p);

void test_free() {
  int *p = (int *)malloc_(sizeof(int));
  *p = 10;
  free_(p);
  int y = *p;
  _mopsa_assert_unsafe();
}




void* weak_malloc_(size_t n) {
  void *strong = malloc_(n);
  void *weak = strong;
  for(int i = 0; i < 100; i++) {
    weak = strong;
    strong = malloc_(n);
  }
  return weak;
}

void test_weak_alloc() {
  char *p = weak_malloc_(100);
  p[0] = 1;
  p[0] = 2;
  _mopsa_assert_exists(p[0] == 1);
  _mopsa_assert_exists(p[0] == 2);
}


/*$
 * assigns: a[0,1];
 * ensures: (a[0])' == 10;
 */
void modify_two_first_elements(char *a);

void test_assign_weak_alloc() {
  char *p = weak_malloc_(100);
  p[0] = 1;
  p[1] = 2;
  p[2] = 3;
  modify_two_first_elements(p);
  _mopsa_assert_exists(p[0] == 1);
  _mopsa_assert_exists(p[0] == 10);
  _mopsa_assert_exists(p[1] == 2);
  _mopsa_assert_exists(p[1] == 100);
  _mopsa_assert(p[2] == 3);
}
