#include <wchar.h>
#include "std_testcase.h"

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


void printLine(const char * line) {return;}

void printWLine(const wchar_t * line) {return;}

void printIntLine (int intNumber) {return;}

void printShortLine (short shortNumber) {return;}

void printFloatLine (float floatNumber) {return;}

void printLongLine(long longNumber) {return;}

void printLongLongLine(int64_t longLongIntNumber) {return;}

void printSizeTLine(size_t sizeTNumber) {return;}

void printHexCharLine(char charHex) {return;}

void printWcharLine(wchar_t wideChar) {return;}

void printUnsignedLine(unsigned unsignedNumber) {return;}

void printHexUnsignedCharLine(unsigned char unsignedCharacter) {return;}

void printDoubleLine(double doubleNumber) {return;}

void printStructLine(const twoIntsStruct * structTwoIntsStruct) {return;}

void printBytesLine(const unsigned char * bytes, size_t numBytes) {return;}
