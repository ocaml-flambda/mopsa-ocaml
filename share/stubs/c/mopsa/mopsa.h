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

// Abstract values
extern int _mopsa_rand();
extern long int _mopsa_rand_int(long int l, long int u);
extern unsigned long int _mopsa_rand_unsigned_long(unsigned long int l, unsigned long int u);

extern char _mopsa_range_char();
extern unsigned char _mopsa_range_unsigned_char();
extern int _mopsa_range_int();
extern unsigned int _mopsa_range_unsigned_int();
extern short _mopsa_range_short();
extern unsigned short _mopsa_range_unsigned_short();
extern long _mopsa_range_long();
extern unsigned long _mopsa_range_unsigned_long();
extern long _mopsa_range_long_long();
extern unsigned long _mopsa_range_unsigned_long_long();


// Raise Exception.Panic exception with a given message
extern void _mopsa_panic(const char* msg);

// Printing
extern void _mopsa_print();


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
