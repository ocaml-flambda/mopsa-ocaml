#ifndef _MOPSA_H
#define _MOPSA_H

// Abstract values
extern long int _mopsa_rand_int(long int, long int);

// Raise Framework.Manager.Panic exception with a given message
extern void _mopsa_panic(const char*);

// Errors
#define OUT_OF_BOUND 1
#define NULL_DEREF 2
#define INVALID_DEREF 3

// Assertions
extern void _mopsa_assert_true(int cond);
extern void _mopsa_assert_exists(int cond);
extern void _mopsa_assert_false(int cond);
extern void _mopsa_assert_unreachable();
extern void _mopsa_assert_safe();
extern void _mopsa_assert_unsafe();
extern void _mopsa_assert_error(int error);
extern void _mopsa_assert_error_at_line(int error, int line);



#endif //_MOPSA_H
