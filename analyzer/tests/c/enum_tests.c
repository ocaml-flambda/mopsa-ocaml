#include "mopsa.h"

enum color { WHITE, BLACK, GREEN, RED, BLUE, YELLOW };
enum KEYS { LEFT = 1, RIGHT, UP = 4, DOWN };

void test_enum_auto_starts_from_zero() {
  _mopsa_assert_true(WHITE == 0);
}

void test_enum_next_value() {
  _mopsa_assert_true(GREEN == BLACK + 1);
}

void test_enum_specific_start() {
  _mopsa_assert_true(RIGHT == 2);
}
