/*************************************************/
/*                                               */
/* Unit tests for the abstraction of floats in C */
/*                                               */
/*************************************************/

#include "mopsa.h"

/*
 * Test that f1 ∈ [f2 - epsilon, f2 + epsilon]
 */
int equals(float f1, float f2, float epsilon) {
  return (f1 >= f2 - epsilon) && (f1 <= f2 + epsilon);
}

/*
 * Test that f1 ∈ [f2, f3]
 */
int in(float f1, float f2, float f3) {
  return (f1 >= f2) && (f1 <= f3);
}


void test_float_initialization_with_constant() {
  float f = 1.1;
  _mopsa_assert(equals(f, 1.1, 0.0001));
}


void test_float_interval_membership() {
  float pi = 3.14159265359;
  _mopsa_assert(in(pi, 3.1415, 3.1416));
}

void test_float_addition() {
  float f1 = 1.1;
  float f2 = 2.2;
  float epsilon = 0.0001;
  _mopsa_assert(equals(f1 + f2, 3.3, epsilon));
}

void test_float_substraction() {
  float f1 = 1.1;
  float f2 = 2.2;
  float epsilon = 0.0001;
  _mopsa_assert(equals(f1 - f2, -1.1, epsilon));
}

void test_float_multiplication() {
  float f1 = 1.1;
  float f2 = 2.2;
  float epsilon = 0.0001;
  _mopsa_assert(equals(f1 * f2, 2.42, epsilon));
}

void test_float_division() {
  float f1 = 1.1;
  float f2 = 2.2;
  float epsilon = 0.0001;
  _mopsa_assert(equals(f1 / f2, 0.5, epsilon));
}
