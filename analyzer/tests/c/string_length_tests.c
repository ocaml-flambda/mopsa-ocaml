/*
  mopsa-c string_length_tests.c -config c/cell-string-length-itv.json -unittest -debug=print
*/

#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#define TEST_CHAR
//#define TEST_WCHAR

const char* s   = "toto";
const char  a[] = "titi";

const wchar_t* ws = L"totos";
const wchar_t  wa[] = L"tutus";


// only requires string litteral domain

void test_cst_length() {
  _mopsa_assert(strlen(s) == 4);
 }

void test_cst_copy() {
  char buf[100];
  strcpy(buf, s);
}

void test_wide_cst_length() {
  _mopsa_assert(wcslen(ws) == 5);
 }

void test_wide_cst_copy() {
  wchar_t wbuf[100];
  wcscpy(wbuf, ws);
}



#ifdef TEST_CHAR
// requires string length domain for char strings

void test_cst_copy_length() {
  char buf[100];
  strcpy(buf, s);
  _mopsa_assert(strlen(buf) == 4);
}

void test_array_length() {
  _mopsa_assert(strlen(a) == 4);
}

void test_buf_length() {
  char buf[100];
  strcpy(buf, s);
  _mopsa_assert(strlen(buf) == 4);
  strcat(buf, s);
  _mopsa_assert(strlen(buf) == 8);
  strcat(buf, a);
  _mopsa_assert(strlen(buf) == 12);
  buf[2] = 0;
  _mopsa_assert(strlen(buf) == 2);
}

void test_alloc_length() {
  char* buf = malloc(200);
  if (buf) {
    strcpy(buf, s);
    _mopsa_assert(strlen(buf) == 4);
    strcat(buf, s);
    _mopsa_assert(strlen(buf) == 8);
    strcat(buf, a);
    _mopsa_assert(strlen(buf) == 12);
    buf[2] = 0;
    _mopsa_assert(strlen(buf) == 2);
  }
}

#endif


#ifdef TEST_WCHAR
// requires string length domain for wide strings

void test_wide_cst_copy_length() {
  wchar_t buf[100];
  wcscpy(buf, ws);
  _mopsa_assert(wcslen(buf) == 5);
}

void test_wide_array_length() {
  _mopsa_assert(wcslen(wa) == 5);
}

void test_wide_buf_length() {
  wchar_t buf[100];
  wcscpy(buf, ws);
  _mopsa_assert(wcslen(buf) == 5);
  wcscat(buf, ws);
  _mopsa_assert(wcslen(buf) == 10);
  wcscat(buf, wa);
  _mopsa_assert(wcslen(buf) == 15);
  buf[2] = 0;
  _mopsa_assert(wcslen(buf) == 2);
}

void test_wide_alloc_length() {
  wchar_t* buf = malloc(200);
  if (buf) {
    wcscpy(buf, ws);
    _mopsa_assert(wcslen(buf) == 5);
    wcscat(buf, ws);
    _mopsa_assert(wcslen(buf) == 10);
    wcscat(buf, wa);
    _mopsa_assert(wcslen(buf) == 15);
    buf[2] = 0;
    _mopsa_assert(wcslen(buf) == 2);
  }
}

#endif



