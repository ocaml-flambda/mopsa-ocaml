#include <stddef.h>
#include <limits.h>

/*$
 * ensures: _argc in [1, INT_MAX - 1]; // do not go beyound INT_MAX because we will
 *                                     // allocate _argc + 1 pointers in _argv
 */
static int _argc;

/*$
 * local:   char ** buf = new Memory;
 * ensures: size(buf) == _argc + 1; // need to allocate an additional
 *                                  // pointer for the last NULL
 * ensures: _argv == buf;
 */
static char **_argv;

/*$
 * local: char* str = new Memory;
 * ensures: size(str) in [1, INT_MAX]; // there is at least the NUL char at the end
 * // TODO ensures: str[size(str) - 1] == 0;
 * ensures: return == str;
 */
char* _mopsa_new_valid_string();

void _mopsa_init_symbolic_argc_argv() {
  // Add program name
  _argv[0] = _mopsa_new_valid_string();

  /*
   * The following code produces false negative
   * when analyzed with non-relational numeric domains
   */
  #if 0
  int i = 1;

  // Initialize the array with valid strings
  while(i < _argc) {
    _argv[i] = _mopsa_new_valid_string();
    i++;
  }

  // Add the last NULL pointer
  _argv[_argc] = NULL;
  #endif
}


// Temporary fix for missing error function in coreutuils
void error (int __status, int __errnum, const char *__format, ...) { }
