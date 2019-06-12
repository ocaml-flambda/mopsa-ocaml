/****************************************************************************/
/*                                                                          */
/* This file is part of MOPSA, a Modular Open Platform for Static Analysis. */
/*                                                                          */
/* Copyright (C) 2019 The MOPSA Project.                                    */
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

#ifndef _MOPSA_H
#define _MOPSA_H


#include <stdint.h>


// Generators of typed random values
extern int8_t _mopsa_rand_int8();
extern uint8_t _mopsa_rand_uint8();
#define _mopsa_rand_char _mopsa_rand_int8

extern int16_t _mopsa_rand_int16();
extern uint16_t _mopsa_rand_uint16();

extern int32_t _mopsa_rand_int32();
extern uint32_t _mopsa_rand_uint32();
#define _mopsa_rand_int _mopsa_rand_int32

extern float _mopsa_rand_float();
extern double _mopsa_rand_double();

extern void *_mopsa_rand_void_pointer();


// Generic generators accepting the type as argument
#define _mopsa_rand_number(T) _mopsa_rand_T()
#define _mopsa_rand_pointer(T) (T)_mopsa_rand_void_pointer()


// Generators of typed random ranges
extern int8_t _mopsa_range_int8(int8_t l, int8_t u);
extern uint8_t _mopsa_range_uint8(uint8_t l, uint8_t u);
#define _mopsa_range_char _mopsa_range_int8

extern int16_t _mopsa_range_int16(int16_t l, int16_t u);
extern uint16_t _mopsa_range_uint16(uint16_t l, uint16_t u);

extern int32_t _mopsa_range_int32(int32_t l, int32_t u);
extern uint32_t _mopsa_range_uint32(uint32_t l, uint32_t u);
#define _mopsa_range_int _mopsa_range_int32

extern float _mopsa_range_float(float l, float u);
extern double _mopsa_range_double(double l, double u);
#define float_t float
#define double_t double

// Generic generators accepting the type as argument
#define _mopsa_range(T,l,u) _mopsa_range_T(T_t l, T_t u)


// Invalid pointer
extern void *_mopsa_invalid_pointer();

// Raise Exception.Panic exception with a given message
extern void _mopsa_panic(const char* msg);


// Printing
extern void _mopsa_print();


// Filters
extern void _mopsa_assume(int cond);



// Errors
#define OUT_OF_BOUND 1
#define NULL_DEREF 2
#define INVALID_DEREF 3
#define INTEGER_OVERFLOW 4
#define DIVISION_BY_ZERO 5


// Assertions
extern void _mopsa_assert(int cond);
extern void _mopsa_assert_exists(int cond);
extern void _mopsa_assert_unreachable();
extern void _mopsa_assert_safe();
extern void _mopsa_assert_unsafe();
extern void _mopsa_assert_error(int error);
extern void _mopsa_assert_error_exists(int error);
extern void _mopsa_assert_error_at_line(int error, int line);


#endif //_MOPSA_H
