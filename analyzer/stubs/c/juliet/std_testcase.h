#ifndef _STD_TESTCASE_H
#define _STD_TESTCASE_H

#include <limits.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mopsa.h"

#define true 1
#define false 0


#define URAND31() (((unsigned)rand()<<30) ^ ((unsigned)rand()<<15) ^ rand())
#define RAND32() (_mopsa_range_int())

// Following is the Juliet definition of RAND32()
/* ((int)(rand() & 1 ? URAND31() : -URAND31() - 1)) */

#define URAND63() (((uint64_t)rand()<<60) ^ ((uint64_t)rand()<<45) ^ ((uint64_t)rand()<<30) ^ ((uint64_t)rand()<<15) ^ rand())
#define RAND64() (_mopsa_range_long())
// Following is the Juliet definition of RAND32()
/* ((int64_t)(rand() & 1 ? URAND63() : -URAND63() - 1)) */

extern const int GLOBAL_CONST_TRUE;
extern const int GLOBAL_CONST_FALSE;
extern const int GLOBAL_CONST_FIVE;

extern int globalTrue;
extern int globalFalse;
extern int globalFive;

typedef struct _twoIntsStruct
{
    int intOne;
    int intTwo;
} twoIntsStruct;


#include "std_testcase_io.h"

#endif
