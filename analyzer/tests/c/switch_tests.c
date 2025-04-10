#include "mopsa.h"


void test_int_condition() {
  int i = 2;
  int j;
  switch (i * 2) {
  case 0: j = 10; break;
  case 2: j = 20; break;
  case 4: j = 30; break;
  case 6: j = 40; break;
  default: j = 40;
  }
  _mopsa_assert(j == 30);
}

void test_char_condition() {
  char s[] = "ab";
  int i = 0, j;
  switch (s[i]) {
  case 'a': j = 0; break;
  case 'b': j = 10; break;
  case 'c': j = 20; break;
  case 'd': j = 30; break;
  default: j = 40;
  }
  _mopsa_assert(j == 0);
}

void test_default_case() {
  int i = 2, j;
  switch (i * 5) {
  case 0: 
  case 1: 
  case 2: 
  case 3: 
    j = ((i * 5)+1) * 10;
    break;
  default: j = 50;
  }
  _mopsa_assert(j == 50);
}

void test_case_with_no_break() {
  int i = 2, j = 0;
  switch (i * 5) {
  case 0: j = j + 1;
  case 5: j = j + 2;
  case 10: j = j + 3;
  case 15: j = j + 4; break;
  default: j = -1;
  }
  _mopsa_assert(j == 7);
}

void test_no_default() {
  int i = 2, j = 0;
  switch (i * 5) {
  case 0: j = 10; break;
  case 1: j = 20; break;
  case 2: j = 30; break;
  case 3: j = 40; break;
  }
  _mopsa_assert(j == 0);
}


void test_duff_device() {
  int to, from = 0;
  int count = 10;
  int n = (count + 7) / 8;
  switch (count % 8) {
  case 0: do { to = from++;
    case 7:      to = from++;
    case 6:      to = from++;
    case 5:      to = from++;
    case 4:      to = from++;
    case 3:      to = from++;
    case 2:      to = from++;
    case 1:      to = from++;
    n = n - 1; } while (n > 0);
  }
  _mopsa_assert(from == 10);
}


#define DIGIT				   \
   case '0': case '1': case '2': case '3': \
   case '4': case '5': case '6': case '7': \
   case '8': case '9'

int test_cases_with_macro() {
  char c = _mopsa_range_char('4','7');
  int x;
  switch (c) {
  DIGIT:
    x = 1;
    break;
  default:
    x = 2;
    break;
  }
  _mopsa_assert(x == 1);
}

int x;

int sidef() { return ++x; }

int test_sideeffect() {
  x = 0;
  int r = 0;
  switch(sidef(x)) {
  case 0:
      r = 0;
      break;
  case 1:
      r = 1;
      break;
  case 2:
      r = 2;
      break;
  }
  _mopsa_assert(r == 1);
  _mopsa_assert(x == 1);
}

void test_gcc_range_in() {
  int i = 1, j;
  switch (i * 5) {
  case 0:
    j = 10;
    break;
  case 1 ... 9:
    j = 20;
    break;
  case 10:
    j = 40;
    break;
  default:
    j = 50;
  }
  _mopsa_assert(j == 20);
}

void test_gcc_range_out() {
  int i = 2, j;
  switch (i * 5) {
  case 0:
    j = 10;
    break;
  case 1 ... 9:
    j = 20;
    break;
  case 10:
    j = 40;
    break;
  default:
    j = 50;
  }
  _mopsa_assert(j == 40);
}
