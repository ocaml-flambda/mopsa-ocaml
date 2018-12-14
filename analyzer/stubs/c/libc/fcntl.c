/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <fcntl.h>
#include "mopsa_libc_utils.h"


/*$
 * requires: __fd in FileDescriptor;
 * // TODO: check __cmd & handle variable arguments
 */
int fcntl (int __fd, int __cmd, ...);

/*$
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 *
 * case "success":
 *   local:   void* fd = new FileDescriptor;
 *   ensures: return == (int)fd;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int open (const char *__file, int __oflag, ...);

#ifdef __USE_ATFILE
/*$
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 * requires: __fd in FileDescriptor;
 *
 * case "success":
 *   local:   void* fd = new FileDescriptor;
 *   ensures: return == (int)fd;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int openat (int __fd, const char *__file, int __oflag, ...);

#endif


/*$
 * requires: exists int i in [0, size(__file) - 1]: __file[i] == 0;
 *
 * case "success":
 *   local:   void* fd = new FileDescriptor;
 *   ensures: return == (int)fd;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int creat (const char *__file, mode_t __mode);

static const int _F_ULOCKF = F_ULOCK;
static const int _F_LOCK = F_LOCK;
static const int _F_TLOCK = F_TLOCK;
static const int _F_TEST = F_TEST;

/*$
 * requires: __fd in FileDescriptor;
 * requires: __cmd >= _F_ULOCKF && __cmd <= _F_TEST;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int lockf (int __fd, int __cmd, off_t __len);

#ifdef __USE_XOPEN2K

static const int _POSIX_FADV_NORMAL = POSIX_FADV_NORMAL;
static const int _POSIX_FADV_NOREUSE = POSIX_FADV_NOREUSE;

/*$
 * requires: __fd in FileDescriptor;
 * requires: __advise >= _POSIX_FADV_NORMAL and __advise <= _POSIX_FADV_NOREUSE;
 */
int posix_fadvise (int __fd, off_t __offset, off_t __len, int __advise);

/*$
 * requires: __fd in FileDescriptor;
 */
int posix_fallocate (int __fd, off_t __offset, off_t __len);

#endif
