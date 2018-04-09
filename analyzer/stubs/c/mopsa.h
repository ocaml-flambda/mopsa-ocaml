#define OUT_OF_BOUND 1
#define NULL_DEREF 2
#define INVALID_DEREF 3

void _mopsa_assert_true(int cond);
void _mopsa_assert_false(int cond);
void _mopsa_assert_unreachable();
void _mopsa_assert_safe();
void _mopsa_assert_error(int error);
void _mopsa_assert_error_at_line(int error, int line);
