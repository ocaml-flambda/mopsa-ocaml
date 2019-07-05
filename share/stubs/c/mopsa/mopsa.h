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

// Type shortcuts
#define u8 unsigned char
#define s8 signed char
#define u16 unsigned short
#define s16 signed short
#define u32 unsigned int
#define s32 signed int
#define u64 unsigned long
#define s64 signed long


// Generators of typed random values
extern s8 _mopsa_rand_s8();
extern u8 _mopsa_rand_u8();
#define _mopsa_rand_char _mopsa_rand_s8

extern s16 _mopsa_rand_s16();
extern u16 _mopsa_rand_u16();

extern s32 _mopsa_rand_s32();
extern u32 _mopsa_rand_u32();
#define _mopsa_rand_int _mopsa_rand_s32

extern s64 _mopsa_rand_s64();
extern u64 _mopsa_rand_u64();

extern float _mopsa_rand_float();
extern double _mopsa_rand_double();

extern void *_mopsa_rand_void_pointer();


// Generic generators accepting the type as argument
#define _mopsa_rand_number(T) _mopsa_rand_T()
#define _mopsa_rand_pointer(T) (T)_mopsa_rand_void_pointer()


// Generators of typed random ranges
extern s8 _mopsa_range_s8(s8 l, s8 u);
extern u8 _mopsa_range_u8(u8 l, u8 u);
#define _mopsa_range_char _mopsa_range_s8

extern s16 _mopsa_range_s16(s16 l, s16 u);
extern u16 _mopsa_range_u16(u16 l, u16 u);

extern s32 _mopsa_range_s32(s32 l, s32 u);
extern u32 _mopsa_range_u32(u32 l, u32 u);
#define _mopsa_range_int _mopsa_range_s32

extern s64 _mopsa_range_s64(s64 l, s64 u);
extern u64 _mopsa_range_u64(u64 l, u64 u);

extern float _mopsa_range_float(float l, float u);
extern double _mopsa_range_double(double l, double u);

// Generic generators accepting the type as argument
#define _mopsa_range(T,l,u) _mopsa_range_T(T l, T u)


// Invalid pointer
extern void *_mopsa_invalid_pointer();

// Raise Exception.Panic exception with a given message
extern void _mopsa_panic(const char* msg);


// Printing
extern void _mopsa_print();


// Filters
extern void _mopsa_assume(int cond);

// Assertions
extern void _mopsa_assert(int cond);
extern void _mopsa_assert_exists(int cond);
extern void _mopsa_assert_unreachable();
extern void _mopsa_assert_safe();
extern void _mopsa_assert_unsafe();


#endif //_MOPSA_H
