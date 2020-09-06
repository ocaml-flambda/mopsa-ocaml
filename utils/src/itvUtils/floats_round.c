/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2017-2019 The MOPSA Project.                               */
/*                                                                          */
/* This program is free software: you can redistribute it and/or modify     */
/* it under the terms of the GNU Lesser General Public License as published */
/* by the Free Software Foundation, either version 3 of the License, or     */
/* (at your option) any later version.                                      */
/*                                                                          */
/* This program is distributed in the hope that it will be useful,          */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/* GNU Lesser General Public License for more details.                      */
/*                                                                          */
/* You should have received a copy of the GNU Lesser General Public License */
/* along with this program.  If not, see <http://www.gnu.org/licenses/>.    */
/*                                                                          */
/****************************************************************************/

/*
  Floating-point arithmetics and interval arthmetics with rounding.

  To minimize rounding mode changes, we simulate rounding downwards
  as rounding upwards the negation of the expected result, when possible.
  Thus, we expect that, in practice, we will only switch between
  rounding upwards and rounding to nearest. Switching between two
  rounding modes is rather efficient on modern processors (but not 
  switching between three or more rounding modes).
 */


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <fenv.h>



/* Compiler checking */
/* ***************** */


/*
  We rely on the compiler strictly evaluating float expressions in single 
  precision and double expressions in double precision in the current
  rounding more, and interpreting (float) casts as rounding to float 
  in the current rounding mode.
  We check some C99 macros at compile time to ensure this.
*/

#if !defined(FLT_EVAL_METHOD) || (FLT_EVAL_METHOD != 0)
#error "C99's FLT_EVAL_METHOD should be 0 to ensure that float is really float and double is really double"
#endif


/* Set rounding direction (internal) */
/* ********************************* */


#if defined(__x86_64__) && defined(__GNUC__)

/*
  Specific path for x86-64 with GCC inline assembly.

  We are trying to be more efficient by directly storing a constant
  into MSCSR instead of using the usual load-modify-store.
  As a consquence, we not only change the rounding mode, but also reset
  other bits (including exception masks and flags, flush-to-zero and
  denormal-are-zero).
  Assumes that float arithmetic is compiled to SSE instructions and does
  not change the x87 FPU.
*/

static const unsigned mxcsr_near = 0x1f80;
static const unsigned mxcsr_down = 0x3f80;
static const unsigned mxcsr_up   = 0x5f80;
static const unsigned mxcsr_zero = 0x7f80;

#define ROUND_NEAR                              \
  asm ("ldmxcsr %0" : : "m" (*&mxcsr_near))

#define ROUND_ZERO                              \
  asm ("ldmxcsr %0" : : "m" (*&mxcsr_zero))

#define ROUND_UP                                \
  asm ("ldmxcsr %0" : : "m" (*&mxcsr_up))

#define ROUND_DOWN                              \
  asm ("ldmxcsr %0" : : "m" (*&mxcsr_down))

#else

/*
  Generic path using fesetround.

  This should work for any C99 compiler.
*/

#define FESETROUND(X)                           \
  if (fesetround((X))) { fprintf(stderr,"Could not set FPU rounding direction: fesetround failed.\n"); exit(1); }

#define ROUND_NEAR                              \
  FESETROUND(FE_TONEAREST)

#define ROUND_ZERO                              \
  FESETROUND(FE_TOWARDZERO)

#define ROUND_UP                                \
  FESETROUND(FE_UPWARD)

#define ROUND_DOWN                              \
  FESETROUND(FE_DOWNWARD)


#endif



/* global rounding */
/* *************** */

CAMLprim value ml_round_near(value unit) {
  ROUND_NEAR;
  return Val_unit;
}

CAMLprim value ml_round_zero(value unit) {
  ROUND_ZERO;
  return Val_unit;
}

CAMLprim value ml_round_up(value unit) {
  ROUND_UP;
  return Val_unit;
}

CAMLprim value ml_round_down(value unit) {
  ROUND_DOWN;
  return Val_unit;
}


  
/* scalar arithmetics */
/* ****************** */

static const double dbl_inf = 1./0.;
static const double dbl_minf = -1./0.;

static const float sgl_inf = 1.f/0.f;
static const float sgl_minf = -1.f/0.f;

/* This multiplication ensures that 0 * infty = 0 */
#define MUL(a,b)  ((a)==0 || (b)==0 ? 0 : (a) * (b))

/* This division ensures that 0 / 0 = infty / infty = 0 */
#define DIV(a,b)  ((a)==0 || isinf((b)) ? 0 : (a) / (b))


/* unary: double -> double */
#define OP_D_D(NAME,CODE)                                       \
  CAMLprim double ml_##NAME##_opt(double a) {                   \
    double r;                                                   \
    CODE;                                                       \
    return r;                                                   \
  }                                                             \
  CAMLprim value ml_##NAME(value aa) {                          \
    CAMLparam1(aa);                                             \
    double a = Double_val(aa), r;                               \
    CODE;                                                       \
    CAMLreturn(caml_copy_double(r));                            \
  }

/* binary: double * double -> double */
#define OP_DD_D(NAME,CODE)                                   \
  CAMLprim double ml_##NAME##_opt(double a, double b) {      \
    double r;                                                \
    CODE;                                                    \
    return r;                                                \
  }                                                          \
  CAMLprim value ml_##NAME(value aa, value bb) {             \
    CAMLparam2(aa,bb);                                       \
    double a = Double_val(aa), b = Double_val(bb), r;        \
    CODE;                                                    \
    CAMLreturn(caml_copy_double(r));                         \
  }

/* unary: double -> value */
#define OP_D_V(NAME,CODE)                                       \
  CAMLprim value ml_##NAME##_opt(double a) {                    \
    value r;                                                    \
    CODE;                                                       \
    return r;                                                   \
  }                                                             \
  CAMLprim value ml_##NAME(value aa) {                          \
    CAMLparam1(aa);                                             \
    double a = Double_val(aa);                                  \
    value r;                                                    \
    CODE;                                                       \
    CAMLreturn(r);                                              \
  }

/* unary: value -> double */
#define OP_V_D(NAME,CODE)                                       \
  CAMLprim double ml_##NAME##_opt(value a) {                    \
    double r;                                                   \
    CODE;                                                       \
    return r;                                                   \
  }                                                             \
  CAMLprim value ml_##NAME(value a) {                           \
    CAMLparam1(a);                                              \
    double r;                                                   \
    CODE;                                                       \
    CAMLreturn(caml_copy_double(r));                            \
  }



/* add */
OP_DD_D(add_dbl_near, { ROUND_NEAR; r = a + b; })
OP_DD_D(add_dbl_up,   { ROUND_UP;   r = a + b; })
OP_DD_D(add_dbl_down, { ROUND_UP;   a = -a; r = -(a - b); })
OP_DD_D(add_dbl_zero, { ROUND_ZERO; r = a + b; })
OP_DD_D(add_sgl_near, { ROUND_NEAR; r = (float)a + (float)b; })
OP_DD_D(add_sgl_up,   { ROUND_UP;   r = (float)a + (float)b; })
OP_DD_D(add_sgl_down, { ROUND_UP;   a = -a; r = -((float)a - (float)b); })
OP_DD_D(add_sgl_zero, { ROUND_ZERO; r = (float)a + (float)b; })

/* sub */
OP_DD_D(sub_dbl_near, { ROUND_NEAR; r = a - b; })
OP_DD_D(sub_dbl_up,   { ROUND_UP;   r = a - b; })
OP_DD_D(sub_dbl_down, { ROUND_UP;   a = -a; r = -(a + b); })
OP_DD_D(sub_dbl_zero, { ROUND_ZERO; r = a - b; })
OP_DD_D(sub_sgl_near, { ROUND_NEAR; r = (float)a - (float)b; })
OP_DD_D(sub_sgl_up,   { ROUND_UP;   r = (float)a - (float)b; })
OP_DD_D(sub_sgl_down, { ROUND_UP;   a = -a; r = -((float)a + (float)b); })
OP_DD_D(sub_sgl_zero, { ROUND_ZERO; r = (float)a - (float)b; })

/* mul */
OP_DD_D(mul_dbl_near, { ROUND_NEAR; r = a * b; })
OP_DD_D(mul_dbl_up,   { ROUND_UP;   r = a * b; })
OP_DD_D(mul_dbl_down, { ROUND_UP;   a = -a; r = -(a * b); })
OP_DD_D(mul_dbl_zero, { ROUND_ZERO; r = a * b; })
OP_DD_D(mul_sgl_near, { ROUND_NEAR; r = (float)a * (float)b; })
OP_DD_D(mul_sgl_up,   { ROUND_UP;   r = (float)a * (float)b; })
OP_DD_D(mul_sgl_down, { ROUND_UP;   a = -a; r = -((float)a * (float)b); })
OP_DD_D(mul_sgl_zero, { ROUND_ZERO; r = (float)a * (float)b; })

/* mul with 0 * infty = 0 */
OP_DD_D(mulz_dbl_near, { ROUND_NEAR; r = MUL(a,b); })
OP_DD_D(mulz_dbl_up,   { ROUND_UP;   r = MUL(a,b); })
OP_DD_D(mulz_dbl_down, { ROUND_UP;   a = -a; r = -MUL(a,b); })
OP_DD_D(mulz_dbl_zero, { ROUND_ZERO; r = MUL(a,b); })
OP_DD_D(mulz_sgl_near, { ROUND_NEAR; float aa = a; float bb = b; r = MUL(aa,bb); })
OP_DD_D(mulz_sgl_up,   { ROUND_UP;   float aa = a; float bb = b; r = MUL(aa,bb); })
OP_DD_D(mulz_sgl_down, { ROUND_UP;   float aa = -a; float bb = b; r = -MUL(aa,bb); })
OP_DD_D(mulz_sgl_zero, { ROUND_ZERO; float aa = a; float bb = b; r = MUL(aa,bb); })

/* div */
OP_DD_D(div_dbl_near, { ROUND_NEAR; r = a / b; })
OP_DD_D(div_dbl_up,   { ROUND_UP;   r = a / b; })
OP_DD_D(div_dbl_down, { ROUND_UP;   a = -a; r = -(a / b); })
OP_DD_D(div_dbl_zero, { ROUND_ZERO; r = a / b; })
OP_DD_D(div_sgl_near, { ROUND_NEAR; r = (float)a / (float)b; })
OP_DD_D(div_sgl_up,   { ROUND_UP;   r = (float)a / (float)b; })
OP_DD_D(div_sgl_down, { ROUND_UP;   a = -a; r = -((float)a / (float)b); })
OP_DD_D(div_sgl_zero, { ROUND_ZERO; r = (float)a / (float)b; })

/* div with 0 / 0 = 0 */
OP_DD_D(divz_dbl_near, { ROUND_NEAR; r = DIV(a,b); })
OP_DD_D(divz_dbl_up,   { ROUND_UP;   r = DIV(a,b); })
OP_DD_D(divz_dbl_down, { ROUND_UP;   a = -a; r = -DIV(a,b); })
OP_DD_D(divz_dbl_zero, { ROUND_ZERO; r = DIV(a,b); })
OP_DD_D(divz_sgl_near, { ROUND_NEAR; float aa = a; float bb = b; r = DIV(aa,bb); })
OP_DD_D(divz_sgl_up,   { ROUND_UP;   float aa = a; float bb = b; r = DIV(aa,bb); })
OP_DD_D(divz_sgl_down, { ROUND_UP;   float aa = -a; float bb = b; r = -DIV(aa,bb); })
OP_DD_D(divz_sgl_zero, { ROUND_ZERO; float aa = a; float bb = b; r = DIV(aa,bb); })

/* mod */
OP_DD_D(mod_dbl_near, { ROUND_NEAR; r = fmod(a, b); })
OP_DD_D(mod_dbl_up,   { ROUND_UP;   r = fmod(a, b); })
OP_DD_D(mod_dbl_down, { ROUND_UP;   r = -fmod(-a, b); })
OP_DD_D(mod_dbl_zero, { ROUND_ZERO; r = fmod(a, b); })
OP_DD_D(mod_sgl_near, { ROUND_NEAR; r = fmodf((float)a, (float)b); })
OP_DD_D(mod_sgl_up,   { ROUND_UP;   r = fmodf((float)a, (float)b); })
OP_DD_D(mod_sgl_down, { ROUND_UP;   a = -a; r = -(fmodf((float)a, (float)b)); })
OP_DD_D(mod_sgl_zero, { ROUND_ZERO; r = fmodf((float)a, (float)b); })

/* sqrt */
OP_D_D(sqrt_dbl_near, { ROUND_NEAR; r = sqrt(a); })
OP_D_D(sqrt_dbl_up,   { ROUND_UP;   r = sqrt(a); })
OP_D_D(sqrt_dbl_down, { ROUND_DOWN; r = sqrt(a); })
OP_D_D(sqrt_dbl_zero, { ROUND_ZERO; r = sqrt(a); })
OP_D_D(sqrt_sgl_near, { ROUND_NEAR; r = sqrtf((float)a); })
OP_D_D(sqrt_sgl_up,   { ROUND_UP;   r = sqrtf((float)a); })
OP_D_D(sqrt_sgl_down, { ROUND_DOWN; r = sqrtf((float)a); })
OP_D_D(sqrt_sgl_zero, { ROUND_ZERO; r = sqrtf((float)a); })

/* cast to float */
OP_D_D(to_sgl_near, { ROUND_NEAR; r = (float)a; })
OP_D_D(to_sgl_up,   { ROUND_UP;   r = (float)a; })
OP_D_D(to_sgl_down, { ROUND_UP;   a = -a; r = -((float)a); })
OP_D_D(to_sgl_zero, { ROUND_ZERO; r = (float)a; })

/* round to integer (returns a float) */
OP_D_D(round_int_dbl_near, { ROUND_NEAR; r = rint(a); })
OP_D_D(round_int_dbl_up,   { ROUND_UP;   r = rint(a); })
OP_D_D(round_int_dbl_down, { ROUND_UP;   a = -a; r = -rint(a); })
OP_D_D(round_int_dbl_zero, { ROUND_ZERO; r = rint(a); })
OP_D_D(round_int_sgl_near, { ROUND_NEAR; r = rintf((float)a); })
OP_D_D(round_int_sgl_up,   { ROUND_UP;   r = rintf((float)a); })
OP_D_D(round_int_sgl_down, { ROUND_UP;   a = -a; r = -(rintf((float)a)); })
OP_D_D(round_int_sgl_zero, { ROUND_ZERO; r = rintf((float)a); })

/* cast to int64 */
OP_D_V(to_int64_near, { ROUND_NEAR; r = caml_copy_int64(llrint(a)); })
OP_D_V(to_int64_up,   { ROUND_UP;   r = caml_copy_int64(llrint(a)); })
OP_D_V(to_int64_down, { ROUND_UP;   a = -a; r = caml_copy_int64(-llrint(a)); })
OP_D_V(to_int64_zero, { r = caml_copy_int64(a); }) // default C semantics is truncation

/* cast from int64 */
OP_V_D(of_int64_dbl_near, { ROUND_NEAR; r = Int64_val(a); })
OP_V_D(of_int64_dbl_up,   { ROUND_UP;   r = Int64_val(a); })
OP_V_D(of_int64_dbl_down, { ROUND_UP;   r = -((double)(-Int64_val(a))); })
OP_V_D(of_int64_dbl_zero, { ROUND_ZERO; r = Int64_val(a); })
OP_V_D(of_int64_sgl_near, { ROUND_NEAR; r = (float)Int64_val(a); })
OP_V_D(of_int64_sgl_up,   { ROUND_UP;   r = (float)Int64_val(a); })
OP_V_D(of_int64_sgl_down, { ROUND_UP;   r = -((float)(-Int64_val(a))); })
OP_V_D(of_int64_sgl_zero, { ROUND_ZERO; r = (float)Int64_val(a); })
OP_V_D(of_int64_sgl_cur, { r = (float)Int64_val(a); })

/* cast to int */
OP_D_V(to_int_near, { ROUND_NEAR; r = Val_long(lrint(a)); })
OP_D_V(to_int_up,   { ROUND_UP;   r = Val_long(lrint(a)); })
OP_D_V(to_int_down, { ROUND_UP;   a = -a; r = Val_long(-lrint(a)); })
OP_D_V(to_int_zero, { r = Val_long(a); }) // default C semantics is truncation

/* cast from int */
OP_V_D(of_int_dbl_near, { ROUND_NEAR; r = Long_val(a); })
OP_V_D(of_int_dbl_up,   { ROUND_UP;   r = Long_val(a); })
OP_V_D(of_int_dbl_down, { ROUND_UP;   r = -((double)(-Long_val(a))); })
OP_V_D(of_int_dbl_zero, { ROUND_ZERO; r = Long_val(a); })
OP_V_D(of_int_sgl_near, { ROUND_NEAR; r = (float)Long_val(a); })
OP_V_D(of_int_sgl_up,   { ROUND_UP;   r = (float)Long_val(a); })
OP_V_D(of_int_sgl_down, { ROUND_UP;   r = -((float)(-Long_val(a))); })
OP_V_D(of_int_sgl_zero, { ROUND_ZERO; r = (float)Long_val(a); })
OP_V_D(of_int_sgl_cur,  { r = (float)Long_val(a); })



/* binary representation */
/* ********************* */


CAMLprim value ml_bits_of_double(value d) {
  CAMLparam1(d);
  union { int64_t i; double d; } v;
  v.d = Double_val(d);
  CAMLreturn(caml_copy_int64(v.i));
}

CAMLprim value ml_double_of_bits(value d) {
  CAMLparam1(d);
  union { int64_t i; double d; } v;
  v.i = Int64_val(d);
  CAMLreturn(caml_copy_double(v.d));
}

CAMLprim value ml_bits_of_float(value d) {
  CAMLparam1(d);
  union { int32_t i; float f; } v;
  v.f = Double_val(d);
  CAMLreturn(caml_copy_int32(v.i));
}

CAMLprim value ml_float_of_bits(value d) {
  CAMLparam1(d);
  union { int32_t i; float f; } v;
  v.i = Int32_val(d);
  CAMLreturn(caml_copy_double(v.f));
}



/* interval arithmetics */
/* ******************** */



/* intervals are encoded as struct with two (unboxed) floats  */

#define get_l(x) Double_field((x),0)
#define get_u(x) Double_field((x),1)

#define set_l(x,v) do { double vv = v; if (isnan(vv)) vv = dbl_minf; Store_double_field((x),0,vv); } while (0)
#define set_u(x,v) do { double vv = v; if (isnan(vv)) vv = dbl_inf;  Store_double_field((x),1,vv); } while (0)

/* The semantics of single-precision functions is that the double arguments
   contain numbers with single precision, and we store the resulting
   single precision value into a double precision number.
   The float cast here is only here to ensure that the C compiler will 
   generate float operations, but should not result in any rounding.
   Hence, the rounding mode when extracting the single-precision 
   argument shoud not matter.
 */
#define get_sl(x) ((float)Double_field((x),0))
#define get_su(x) ((float)Double_field((x),1))
#define get_nsl(x) ((float)(-Double_field((x),0)))
#define get_nsu(x) ((float)(-Double_field((x),1)))



/* add */
/* --- */

CAMLprim value ml_add_dbl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  set_l(r, get_l(a) + get_l(b));
  set_u(r, get_u(a) + get_u(b));
  return Val_unit;
}

CAMLprim value ml_add_dbl_itv_up(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_l(a) + get_l(b));
  set_u(r, get_u(a) + get_u(b));
  return Val_unit;
}

CAMLprim value ml_add_dbl_itv_down(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_l(a) - get_l(b)));
  set_u(r, -(- get_u(a) - get_u(b)));
  return Val_unit;
}

CAMLprim value ml_add_dbl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  set_l(r, get_l(a) + get_l(b));
  set_u(r, get_u(a) + get_u(b));
  return Val_unit;
}

CAMLprim value ml_add_dbl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_l(a) - get_l(b)));
  set_u(r, get_u(a) + get_u(b));
  return Val_unit;
}

CAMLprim value ml_add_dbl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_l(a) + get_l(b));
  set_u(r, -(- get_u(a) - get_u(b)));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  set_l(r, get_sl(a) + get_sl(b));
  set_u(r, get_su(a) + get_su(b));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_up(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_sl(a) + get_sl(b));
  set_u(r, get_su(a) + get_su(b));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_down(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(-get_sl(a) - get_sl(b)));
  set_u(r, -(-get_su(a) - get_su(b)));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  set_l(r, get_sl(a) + get_sl(b));
  set_u(r, get_su(a) + get_su(b));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_sl(a) - get_sl(b)));
  set_u(r, get_su(a) + get_su(b));
  return Val_unit;
}

CAMLprim value ml_add_sgl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_sl(a) + get_sl(b));
  set_u(r, -(- get_su(a) - get_su(b)));
  return Val_unit;
}


/* sub */
/* --- */

CAMLprim value ml_sub_dbl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  set_l(r, get_l(a) - get_u(b));
  set_u(r, get_u(a) - get_l(b));
  return Val_unit;
}

CAMLprim value ml_sub_dbl_itv_up(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_l(a) - get_u(b));
  set_u(r, get_u(a) - get_l(b));
  return Val_unit;
}

CAMLprim value ml_sub_dbl_itv_down(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_l(a) + get_u(b)));
  set_u(r, -(- get_u(a) + get_l(b)));
  return Val_unit;
}

CAMLprim value ml_sub_dbl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  set_l(r, get_l(a) - get_u(b));
  set_u(r, get_u(a) - get_l(b));
  return Val_unit;
}

CAMLprim value ml_sub_dbl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_l(a) + get_u(b)));
  set_u(r, get_u(a) - get_l(b));
  return Val_unit;
}

CAMLprim value ml_sub_dbl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_l(a) - get_u(b));
  set_u(r, -(- get_u(a) + get_l(b)));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  set_l(r, get_sl(a) - get_su(b));
  set_u(r, get_su(a) - get_sl(b));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_up(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_sl(a) - get_su(b));
  set_u(r, get_su(a) - get_sl(b));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_down(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_sl(a) + get_su(b)));
  set_u(r, -(- get_su(a) + get_sl(b)));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  set_l(r, get_sl(a) - get_su(b));
  set_u(r, get_su(a) - get_sl(b));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  set_l(r, -(- get_sl(a) + get_su(b)));
  set_u(r, get_su(a) - get_sl(b));
  return Val_unit;
}

CAMLprim value ml_sub_sgl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  set_l(r, get_sl(a) - get_su(b));
  set_u(r, -(- get_su(a) + get_sl(b)));
  return Val_unit;
}


/* mul */
/* --- */

CAMLprim value ml_mul_dbl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_dbl_itv_up(value a, value b, value r) {
  ROUND_UP;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_dbl_itv_down(value a, value b, value r) {
  ROUND_UP;
  double l1 = -get_l(a), l2 = get_l(b), h1 = -get_u(a), h2 = get_u(b);
  double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_dbl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_dbl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  double l1 = -get_l(a), l2 = get_l(b), h1 = -get_u(a), h2 = get_u(b);
  {
    double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_mul_dbl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  {
    double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    double ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_up(value a, value b, value r) {
  ROUND_UP;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_down(value a, value b, value r) {
  ROUND_UP;
  float l1 = -get_sl(a), l2 = get_sl(b), h1 = -get_su(a), h2 = get_su(b);
  float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  float l1 = -get_sl(a), l2 = get_sl(b), h1 = -get_su(a), h2 = get_su(b);
  {
    float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_mul_sgl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  {
    float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    float ll = MUL(l1,l2), lh = MUL(l1,h2), hl = MUL(h1,l2), hh = MUL(h1,h2);
    set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  return Val_unit;
}


/* divpos */
/* ------ */

/* division by an interval of constant sign */

CAMLprim value ml_divpos_dbl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_dbl_itv_up(value a, value b, value r) {
  ROUND_UP;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_dbl_itv_down(value a, value b, value r) {
  ROUND_UP;
  double l1 = -get_l(a), l2 = get_l(b), h1 = -get_u(a), h2 = get_u(b);
  double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_dbl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_dbl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  double l1 = -get_l(a), l2 = get_l(b), h1 = -get_u(a), h2 = get_u(b);
  {
    double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_divpos_dbl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  double l1 = get_l(a), l2 = get_l(b), h1 = get_u(a), h2 = get_u(b);
  {
    double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    double ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_near(value a, value b, value r) {
  ROUND_NEAR;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_up(value a, value b, value r) {
  ROUND_UP;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_down(value a, value b, value r) {
  ROUND_UP;
  float l1 = -get_sl(a), l2 = get_sl(b), h1 = -get_su(a), h2 = get_su(b);
  float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_zero(value a, value b, value r) {
  ROUND_ZERO;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
  set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_outer(value a, value b, value r) {
  ROUND_UP;
  float l1 = -get_sl(a), l2 = get_sl(b), h1 = -get_su(a), h2 = get_su(b);
  {
    float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_l(r, -fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_u(r, fmax(fmax(ll,hh), fmax(lh,hl)));
  }
  return Val_unit;
}

CAMLprim value ml_divpos_sgl_itv_inner(value a, value b, value r) {
  ROUND_UP;
  float l1 = get_sl(a), l2 = get_sl(b), h1 = get_su(a), h2 = get_su(b);
  {
    float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_l(r, fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  l1 = -l1; h1 = -h1;
  {
    float ll = DIV(l1,l2), lh = DIV(l1,h2), hl = DIV(h1,l2), hh = DIV(h1,h2);
    set_u(r, -fmin(fmin(ll,hh), fmin(lh,hl)));
  }
  return Val_unit;
}



/* conversion with string */
/* ---------------------- */


#define SKIP_SPACE(s)                           \
  while (*(s) == ' ') (s)++

static double parse_string(int up, char* s)
{
  int neg = 0;
  int exponent = 0;
  double mantissa = 0;
  SKIP_SPACE(s);
  /* sign */
  if (*s == '+') s++;
  if (*s == '-') { neg = 1; s++; }
  /* setup rounding */
  if ((up && !neg) || (!up && neg)) {
    ROUND_UP;
  }
  else {
    ROUND_DOWN;
  }
  /* integer mantissa */
  SKIP_SPACE(s);
  while (*s >= '0' && *s <= '9') {
    mantissa = mantissa * 10 + (*s - '0');
    s++;
  }
  /* fractional mantissa */
  SKIP_SPACE(s);
  if (*s == '.') {
    s++;
    SKIP_SPACE(s);
    while (*s >= '0' && *s <= '9') {    
      mantissa = mantissa * 10 + (*s - '0');
      exponent--;
      s++;
    }
  }
  /* exponent */
  SKIP_SPACE(s);
  if (*s == 'e' || *s == 'E') {
    int exp = 0;
    int expsign = 1;
    s++;
    SKIP_SPACE(s);
    if (*s == '-') { expsign = -1; s++; }
    SKIP_SPACE(s);
    while (*s >= '0' && *s <= '9') {    
      exp = exp * 10 + (*s - '0');
      s++;
    }
    exponent += expsign * exp;
  }
  /* NOTE: not sure whether pow obeys ROUND_UP / ROUND_DOWN */
  mantissa *= pow(10., exponent);
  if (neg) mantissa = -mantissa;
  return mantissa;
}

CAMLprim value ml_of_string_dbl_up(value s) {
  CAMLparam1(s);
  double f = parse_string(1, String_val(s));
  CAMLreturn(caml_copy_double(f));
}

CAMLprim value ml_of_string_dbl_down(value s) {
  CAMLparam1(s);
  double f = parse_string(0, String_val(s));
  CAMLreturn(caml_copy_double(f));
}

CAMLprim value ml_of_string_sgl_up(value s) {
  CAMLparam1(s);
  double f = parse_string(1, String_val(s));
  ROUND_UP;
  float r = f;
  CAMLreturn(caml_copy_double(r));
}

CAMLprim value ml_of_string_sgl_down(value s) {
  CAMLparam1(s);
  double f = parse_string(1, String_val(s));
  ROUND_DOWN;
  float r = f;
  CAMLreturn(caml_copy_double(r));
}

