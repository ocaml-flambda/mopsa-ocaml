#ifndef _STDLIB_H
#define _STDLIB_H

#include "mopsa.h"

#define RAND_MAX 2147483647

#define rand() _mopsa_rand_int(0, RAND_MAX)


#endif //_STDLIB_H
