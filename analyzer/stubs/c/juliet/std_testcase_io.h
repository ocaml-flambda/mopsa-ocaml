#ifndef _STD_TESTCASE_IO_H
#define _STD_TESTCASE_IO_H

#include <wchar.h>
#include "std_testcase.h"

int globalReturnsTrue();
int globalReturnsFalse();
int globalReturnsTrueOrFalse();


void printLine(const char * line);
void printWLine(const wchar_t * line);
void printIntLine (int intNumber);
void printShortLine (short shortNumber);
void printFloatLine (float floatNumber);
void printLongLine(long longNumber);
void printLongLongLine(int64_t longLongIntNumber);
void printSizeTLine(size_t sizeTNumber);
void printHexCharLine(char charHex);
void printWcharLine(wchar_t wideChar);
void printUnsignedLine(unsigned unsignedNumber);
void printHexUnsignedCharLine(unsigned char unsignedCharacter);
void printDoubleLine(double doubleNumber);
void printStructLine(const twoIntsStruct * structTwoIntsStruct);
void printBytesLine(const unsigned char * bytes, size_t numBytes);

#endif
