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

#ifndef _ATOMIC_STUBS_H
#define _ATOMIC_STUBS_H

/*
  Replaces Clang's __c11_atomic_ intrisincs with regular (non-atomic) 
  operations to allow analysis until we support concurrency and atomicity 
  in Mopsa.
 */

#define __c11_atomic_init(obj, value)           \
  (*(obj) = (value))

#define __c11_atomic_load(obj, order)           \
  (*(obj))

#define __c11_atomic_store(obj, value, order)   \
  (*(obj) = (value))

/*
  "typeof(*(obj)+0)" is a trick to get the type of the objet pointed by obj
  with _Atomic stripped (to avoid type error in assignment)
 */

#define __c11_atomic_exchange(obj, value, order)                        \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = (value); __tmp; })

#define __c11_atomic_compare_exchange_strong(obj1, obj2, val, order1, order2) \
  ({ int __tmp = *(obj1) == *(obj2);                                    \
    if (__tmp) *(obj1) = (val); else *(obj2) = *(obj1);                 \
    __tmp; })

#define __c11_atomic_compare_exchange_weak(obj1, obj2, val, order1, order2) \
  __c11_atomic_compare_exchange_strong(obj1, obj2, val, order1, order2)

#define __c11_atomic_fetch_add(obj, arg, order)                         \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = __tmp + (arg);__tmp; })

#define __c11_atomic_fetch_sub(obj, arg, order)                         \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = __tmp - (arg); __tmp; })

#define __c11_atomic_fetch_and(obj, arg, order)                         \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = __tmp & (arg); __tmp; })

#define __c11_atomic_fetch_or(obj, arg, order)                          \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = __tmp | (arg); __tmp; })

#define __c11_atomic_fetch_xor(obj, arg, order)                         \
  ({ typeof(*(obj)+0) __tmp = *(obj); *(obj) = __tmp ^ (arg); __tmp; })

#define __c11_atomic_fetch_max(obj, arg, order)                         \
  ({ typeof(*(obj)+0) __tmp = *(obj);                                   \
    *(obj) = __tmp > (arg) ? __tmp : (arg);                             \
    __tmp; })

#define __c11_atomic_fetch_min(obj, arg, order) \
  ({ typeof(*(obj)+0) __tmp = *(obj);           \
    *(obj) = __tmp < (arg) ? __tmp : (arg);     \
    __tmp; })

#endif

