/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


/*
  Useful definitions used throughout the libc stubs.
 */

#ifndef MOPSA_LIBC_UTILS_H
#define MOPSA_LIBC_UTILS_H

extern int _errno;

// Translate a numeric file descriptor into the address of its resource
extern void *_mopsa_int_to_fd(int fd);


/*
  Some useful predicates
*/

/*$$
 * predicate valid_string(s):
 *   exists int i in [0, size(s) - 1]: s[i] == 0
 * ;
 *
 * predicate valid_primed_string(s):
 *   exists int i in [0, size(s) - 1]: (s[i])' == 0
 * ;
 *
 * predicate valid_substring(s, n):
 *   exists int i in [0, n - 1]: s[i] == 0
 * ;
 *
 * predicate valid_primed_substring(s, n):
 *   exists int i in [0, n - 1]: (s[i])' == 0
 * ;
 */



#endif /* MOPSA_LIBC_UTILS_H */
