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


#include <limits.h>


/**************************/
/* Symbolic argc and argv */
/**************************/

/*$
 * ensures: return in [1, INT_MAX - 1];   // Do not go beyound INT_MAX because
 *                                        // we will allocate argc + 1 pointers
 *                                        // in argv
 */
static int _mopsa_init_symbolic_argc();


/*$
 * local:   char ** argv = new Memory;
 * ensures: size(argv) == argc + 1;      // Need to allocate one additional
 *                                       // pointer for the last NULL
 * ensures: forall int i in [0, argc]:
 *            argv[i] == NULL;           // Ugly trick! ensure that all values
 *                                       // are set to NULL to avoid the default
 *                                       // ⊤ value generated after the allocation
 * ensures: return == argv;
 */
static char **_mopsa_init_symbolic_argv(int argc);
