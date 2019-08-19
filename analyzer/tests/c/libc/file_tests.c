/*
 * Tests of file functions
 */

#include <stdio.h>
#include <unistd.h>

void test_standard_streams_are_not_null() {
  _mopsa_assert(stdin != NULL);
  _mopsa_assert(stdout != NULL);
  _mopsa_assert(stderr != NULL);
}

void test_fileno_of_standard_streams() {
  _mopsa_assert(fileno(stdin) == STDIN_FILENO);
  _mopsa_assert(fileno(stdout) == STDOUT_FILENO);
  _mopsa_assert(fileno(stderr) == STDERR_FILENO);
}
