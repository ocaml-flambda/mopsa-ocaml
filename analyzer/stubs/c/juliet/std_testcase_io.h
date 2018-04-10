#ifndef _STD_TESTCASE_IO_H
#define _STD_TESTCASE_IO_H

#include <stdlib.h>

#define true 1
#define false 0

#define URAND31() (((unsigned)rand()<<30) ^ ((unsigned)rand()<<15) ^ rand())
#define RAND32() ((int)(rand() & 1 ? URAND31() : -URAND31() - 1))
#define URAND63() (((uint64_t)rand()<<60) ^ ((uint64_t)rand()<<45) ^ ((uint64_t)rand()<<30) ^ ((uint64_t)rand()<<15) ^ rand())
#define RAND64() ((int64_t)(rand() & 1 ? URAND63() : -URAND63() - 1))

const int GLOBAL_CONST_TRUE = 1;
const int GLOBAL_CONST_FALSE = 0;
const int GLOBAL_CONST_FIVE = 5;

int globalTrue = 1;
int globalFalse = 0;
int globalFive = 5; 

int globalReturnsTrue() 
{
    return 1;
}

int globalReturnsFalse() 
{
    return 0;
}


int globalReturnsTrueOrFalse() 
{
    return (rand() % 2);
}

typedef struct _twoIntsStruct
{
    int intOne;
    int intTwo;
} twoIntsStruct;


#endif
