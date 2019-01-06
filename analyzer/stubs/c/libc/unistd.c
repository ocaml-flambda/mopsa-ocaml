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
#include <unistd.h>
#include <limits.h>
#include "mopsa_libc_utils.h"

/*$
 * requires: valid_string(__name);
 * requires: __type in [0, __type];
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int access (const char *__name, int __type);


#ifdef __USE_GNU

/*$
 * local: int r = access(__name, __type);
 * ensures: return == r;
 */
int euidaccess (const char *__name, int __type);

/*$
 * local: int r = access(__name, __type);
 * ensures: return == r;
 */
int eaccess (const char *__name, int __type);

#endif

#ifdef __USE_ATFILE

/*$
 * requires: __fd in FileDescriptor;
 * local: int r = access(__file, __type);
 * ensures: return == r;
 */
int faccessat (int __fd, const char *__file, int __type, int __flag);

#endif

/*$
 * requires: __fd in FileDescriptor;
 * requires: __whence in [0, __whence];
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__off_t lseek (int __fd, __off_t __offset, int __whence);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   free:    __fd;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int close (int __fd);

/*$
 * requires: __fd in FileDescriptor;
 * requires: size(__buf) >= __nbytes;
 *
 * case "success" {
 *   assigns: __buf[0, __nbytes - 1];
 *   ensures: return in [0, __nbytes];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t read (int __fd, void *__buf, size_t __nbytes);

/*$
 * requires: __fd in FileDescriptor;
 * requires: size(__buf) >= __n;
 *
 * case "success" {
 *   ensures: return in [0, __n];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t write (int __fd, const void *__buf, size_t __n);

/*$
 * local:   ssize_t r = read(__fd, __buf, __nbytes);
 * ensures: return == r;
 */
ssize_t pread (int __fd, void *__buf, size_t __nbytes,
               __off_t __offset);

/*$
 * local:   ssize_t r = write(__fd, __buf, __n);
 * ensures: return == r;
 */
ssize_t pwrite (int __fd, const void *__buf, size_t __n,
                __off_t __offset);

/*$
 * case "success" {
 *   local: int fd0 = new FileDescriptor;
 *   local: int fd1 = new FileDescriptor;
 *   assigns: __pipedes[0,1];
 *   ensures: (__pipedes[0])' == fd0;
 *   ensures: (__pipedes[1])' == fd1;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int pipe (int __pipedes[2]);

#ifdef __USE_GNU

/*$
 * local:   int r = pipe(__pipedes);
 * ensures: return == r;
 */
int pipe2 (int __pipedes[2], int __flags);

#endif

/*$
 * warn: "unsupported stub";
 */
unsigned int alarm (unsigned int __seconds);

/*$
 * // empty contract
 */
unsigned int sleep (unsigned int __seconds);

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) || defined __USE_MISC

/*$
 * warn: "unsupported stub";
 * assigns: _errno;
 */
__useconds_t ualarm (__useconds_t __value, __useconds_t __interval);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int usleep (__useconds_t __useconds);

#endif

/*$
 * // NOTE: waits until EINTR, so, always return an error!
 * assigns: _errno;
 * ensures: return == -1;
 */
int pause (void);

/*$
 * requires: valid_string(__file);
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int chown (const char *__file, __uid_t __owner, __gid_t __group);


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * requires: __fd in FileDescriptor;
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fchown (int __fd, __uid_t __owner, __gid_t __group);

/*$
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * local: int r = chown(__file, __owner, __group);
 * ensures: return == r;
 */
int lchown (const char *__file, __uid_t __owner, __gid_t __group);

#endif


#ifdef __USE_ATFILE

/*$
 * requires: __fd in FileDescriptor;
 * requires: valid_string(__file);
 * requires: __owner >= 0;
 * requires: __group >= 0;
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fchownat (int __fd, const char *__file, __uid_t __owner,
              __gid_t __group, int __flag);

#endif

/*$
 * requires: valid_string(__path);
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int chdir (const char *__path);


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * requires: __fd in FileDescriptor;
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fchdir (int __fd);

#endif

/*$
 * requires: __buf != NULL implies size(__buf) >= __size;
 *
 * case "noalloc" {
 *   assumes: __buf != NULL;
 *   assigns: __buf[0, __size - 1];
 *   ensures: valid_primed_substring(__buf, __size);
 *   ensures: return == __buf;
 * }
 *
 * case "toosmall" {
 *   assumes: __buf != NULL;
 *   ensures: return == NULL;
 * }
 *
 * case "alloc" {
 *   assumes: __buf == NULL;
 *   local:   char* r = new Memory;
 *   ensures: return == r;
 *   ensures: size(return) <= PATH_MAX;
 *   ensures: valid_string(return);
 * }
 *   
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *getcwd (char *__buf, size_t __size);

#ifdef	__USE_GNU

/*$
 * case "success" {
 *   local:   char* r = new Memory;
 *   ensures: return == r;
 *   ensures: size(return) <= PATH_MAX;
 *   ensures: valid_string(return);
 * }
 *   
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *get_current_dir_name (void);

#endif

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) || defined __USE_MISC

/*$
 * requires: size(__buf) >= PATH_MAX;
 *
 * case "success" {
 *   assigns: __buf[0, PATH_MAX - 1];
 *   ensures: valid_primed_substring(__buf, PATH_MAX);
 *   ensures: return == __buf;
 * }
 *
 *  case "failure" {
 *    assigns: _errno;
 *    ensures: return == NULL;
 * }
 */
char *getwd (char *__buf);

#endif

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   local:   int r = new FileDescriptor;
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int dup (int __fd);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "newfd" {
 *   warn: "unsupported stub case";
 *   local:   int r = new FileDescriptor;
 *   ensures: r == __fd2;
 *   ensures: return == r;
 * }
 *
 * case "reopen" {
 *   assumes: __fd2 in FileDescriptor;
 *   ensures: return == __fd2;
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int dup2 (int __fd, int __fd2);

#ifdef __USE_GNU

/*$
 * local:   int r = dup2(__fd, __fd2);
 * ensures: return == r;
 */
int dup3 (int __fd, int __fd2, int __flags);

#endif

// TODO: environ / __environ

/*$
 * requires: valid_string(__path);
 * requires: forall int i in [0, size(__argv) - 1]:
 *             valid_string(__argv[i]);
 * requires: forall int i in [0, size(__envp) - 1]:
 *             valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execve (const char *__path, char *const __argv[],
            char *const __envp[]);

#ifdef __USE_XOPEN2K8

/*$
 * requires: __fd in FileDescriptor;
 * requires: forall int i in [0, size(__argv) - 1]:
 *             valid_string(__argv[i]);
 * requires: forall int i in [0, size(__envp) - 1]:
 *             valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int fexecve (int __fd, char *const __argv[], char *const __envp[]);

#endif

/*$
 * requires: valid_string(__path);
 * requires: forall int i in [0, size(__argv) - 1]:
 *             valid_string(__argv[i]);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execv (const char *__path, char *const __argv[]);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__path);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execle (const char *__path, const char *__arg, ...);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__path);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execl (const char *__path, const char *__arg, ...);

/*$
 * requires: valid_string(__file);
 * requires: forall int i in [0, size(__argv) - 1]:
 *             valid_string(__argv[i]);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execvp (const char *__file, char *const __argv[]);

/*$
 * // TODO: variable arguments
 * requires: valid_string(__file);
 * requires: valid_string(__arg);
 *
 * case "success" {
 *   ensures: 0 == 1; // TODO: does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execlp (const char *__file, const char *__arg, ...);

#ifdef __USE_GNU

/*$
 * requires: valid_string(__file);
 * requires: forall int i in [0, size(__argv) - 1]:
 *             valid_string(__argv[i]);
 * requires: forall int i in [0, size(__envp) - 1]:
 *           valid_string(__envp[i]);
 *
 * case "success" {
 *   ensures: 0 == 1; // does not terminate
 * }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int execvpe (const char *__file, char *const __argv[],
             char *const __envp[]);

#endif


#if defined __USE_MISC || defined __USE_XOPEN

/*$
 * case "success" { }
 * 
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
int nice (int __inc);

#endif

/*$
 * ensures: 0 == 1; // does not terminate
 */
void _exit (int __status);

/*$
 * requires: valid_string(__path);
 *
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int pathconf (const char *__path, int __name);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int fpathconf (int __fd, int __name);

/*$
 * case "success" { }
 *
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == -1;
 * }
 */
long int sysconf (int __name);

#ifdef	__USE_POSIX2

/*$
 * requires: __buf != NULL implies size(__buf) >= __len;
 *
 * case "copy" {
 *   assumes: __buf != NULL;
 *   assigns: __buf[0, size(__buf) - 1];
 *   ensures: valid_primed_string(__buf);
 *   ensures: return >= 1; // TODO: upper bound on return
 * }
 *
 * case "null" {
 *   assumes: __buf == NULL;
 *   ensures: return >= 1;
 * }
 *   
 * case "failure" {
 *    assigns: _errno;
 *    ensures: return == 0;
 * }
 */
size_t confstr (int __name, char *__buf, size_t __len);

#endif

/*$
 * ensures: return >= 0;
 */
__pid_t getpid (void);

/*$
 * ensures: return >= 0;
 */
__pid_t getppid (void);

/*$
 * ensures: return >= 0;
 */
__pid_t getpgrp (void);

/*$
 * requires: __pid >= 0;
 * ensures:  return >= 0;
 */
__pid_t __getpgid (__pid_t __pid);

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * requires: __pid >= 0;
 * ensures:  return >= 0;
 */
__pid_t getpgid (__pid_t __pid);

#endif

/*$
 * requires: __pid >= 0;
 * requires: __pgid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setpgid (__pid_t __pid, __pid_t __pgid);

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setpgrp (void);

#endif

/*$
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t setsid (void);

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * requires: __pid >= 0;
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t getsid (__pid_t __pid);

#endif

/*$
 * ensures: return >= 0;
 */
__uid_t getuid (void);

/*$
 * ensures: return >= 0;
 */
__uid_t geteuid (void);

/*$
 * ensures: return >= 0;
 */
__gid_t getgid (void);

/*$
 * ensures: return >= 0;
 */
__gid_t getegid (void);

/*$
 * requires: __size >= 0;
 *
 * case "success" {
 *   assumes: __size > 0;
 *   assigns: __list[0, __size - 1];
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getgroups (int __size, __gid_t __list[]);

#ifdef	__USE_GNU

/*$
 * requires: __gid >= 0;
 */
int group_member (__gid_t __gid);

#endif

/*$
 * requires: __uid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setuid (__uid_t __uid);

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * requires: __ruid >= 0;
 * requires: __euid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setreuid (__uid_t __ruid, __uid_t __euid);

#endif

#ifdef __USE_XOPEN2K

/*$
 * requires: __uid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int seteuid (__uid_t __uid);

#endif

/*$
 * requires: __gid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setgid (__gid_t __gid);

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * requires: __rgid >= 0;
 * requires: __egid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setregid (__gid_t __rgid, __gid_t __egid);

#endif

#ifdef __USE_XOPEN2K

/*$
 * requires: __gid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setegid (__gid_t __gid);

#endif

#ifdef __USE_GNU

/*$
 * case "success" {
 *   assigns: *__ruid;
 *   assigns: *__euid;
 *   assigns: *__suid;
 *   ensures: (*__ruid)' >= 0;
 *   ensures: (*__euid)' >= 0;
 *   ensures: (*__suid)' >= 0;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid);

/*$
 * case "success" {
 *   assigns: *__rgid;
 *   assigns: *__egid;
 *   assigns: *__sgid;
 *   ensures: (*__rgid)' >= 0;
 *   ensures: (*__egid)' >= 0;
 *   ensures: (*__sgid)' >= 0;
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid);

/*$
 * requires: __ruid >= 0;
 * requires: __euid >= 0;
 * requires: __suid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid);

/*$
 * requires: __rgid >= 0;
 * requires: __egid >= 0;
 * requires: __sgid >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid);

#endif

/*$
 * warn: "unsupported stub";
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t fork (void);

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) || defined __USE_MISC

/*$
 * warn: "unsupported stub";
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t vfork (void);

#endif

static char _ttyname_buf[PATH_MAX];

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   assigns: _ttyname_buf[0, PATH_MAX - 1];
 *   ensures: valid_primed_substring(_ttyname_buf, PATH_MAX);
 *   ensures: return == (char*)&_ttyname_buf[0];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *ttyname (int __fd);

/*$
 * requires: __fd in FileDescriptor;
 * requires: size(__buf) >= __buflen;
 *
 * case "success" {
 *   assigns: __buf[0, __buflen - 1];
 *   ensures: valid_primed_substring(__buf, __buflen);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return != 0;
 * }
 */
int ttyname_r (int __fd, char *__buf, size_t __buflen);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 1;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == 0;
 * }
 */
int isatty (int __fd);

#ifdef __USE_MISC

/*$
 * ensures: return >= -1;
 */
int ttyslot (void);

#endif

/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int link (const char *__from, const char *__to);

#ifdef __USE_ATFILE

/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 * requires: __fromfd in FileDescriptor;
 * requires: __tofd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int linkat (int __fromfd, const char *__from, int __tofd,
            const char *__to, int __flags);

#endif

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K

/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int symlink (const char *__from, const char *__to);

/*$
 * requires: valid_string(__path);
 * requires: size(__buf) >= __len;
 *
 * case "success" {
 *   assigns: __buf[0, __len - 1];
 *   ensures: return in [0,__len];
 *   // no 0 added, but returns the position where to put the 0
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t readlink (const char *__restrict __path,
                  char *__restrict __buf, size_t __len);

#endif

#ifdef __USE_ATFILE

/*$
 * requires: valid_string(__from);
 * requires: valid_string(__to);
 * requires: __tofd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int symlinkat (const char *__from, int __tofd,
               const char *__to);

/*$
 * requires: __fd in FileDescriptor;
 * requires: valid_string(__path);
 * requires: size(__buf) >= __len;
 *
 * case "success" {
 *   assigns: __buf[0, __len - 1];
 *   ensures: return in [0, __len];
 *   // no 0 added, but returns the position where to put the 0
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t readlinkat (int __fd, const char *__restrict __path,
                    char *__restrict __buf, size_t __len);

#endif

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int unlink (const char *__name);

#ifdef __USE_ATFILE

/*$
 * requires: __fd in FileDescriptor;
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int unlinkat (int __fd, const char *__name, int __flag);

#endif

/*$
 * requires: valid_string(__path);
 * 
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int rmdir (const char *__path);

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return >= 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
__pid_t tcgetpgrp(int __fd);

/*$
 * requires: __fd in FileDescriptor;
 * requires: __pgrp_id >= 0;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int tcsetpgrp (int __fd, __pid_t __pgrp_id);

static char _getlogin_buf[LOGIN_NAME_MAX];

/*$
 * assigns: _getlogin_buf[0, LOGIN_NAME_MAX - 1];
 * ensures: valid_primed_substring(_getlogin_buf, LOGIN_NAME_MAX);
 * ensures: return == (char*)&_getlogin_buf[0];
 */
char *getlogin (void);

#ifdef __USE_POSIX199506

/*$
 * requires: size(__name) >= __name_len;
 *
 * case "success" {
 *   assigns: __name[0, __name_len - 1];
 *   ensures: valid_primed_substring(__name, __name_len);
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getlogin_r (char *__name, size_t __name_len);

#endif

#ifdef	__USE_MISC

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setlogin (const char *__name);

#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K

/*$
 * requires: size(__name) >= __len;
 *
 * case "success" {
 *   // NOTE: 0 added only if the buffer is large enugh, which we cannot assume
 *   assigns: __name[0, __len - 1];
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int gethostname (char *__name, size_t __len);

#endif


#if defined __USE_MISC

/*$
 * requires: size(__name) >= __len;
 * // NOTE: no terminal 0 required
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sethostname (const char *__name, size_t __len);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int sethostid (long int __id);

/*$
 * requires: size(__name) >= __len;
 *
 * case "success" {
 *   // NOTE: 0 added only if the buffer is large enugh, which we cannot assume
 *   assigns: __name[0, __len - 1];
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getdomainname (char *__name, size_t __len);

/*$
 * requires: size(__name) >= __len;
 * // NOTE: no terminal 0 required
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int setdomainname (const char *__name, size_t __len);

/*$
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int vhangup (void);

/*$
 * warn: "undocumented call";
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int revoke (const char *__file);

/*$
 * warn: "unimplemented call";
 * assigns: _errno;
 * ensures: return == -1;
 */
int profil (unsigned short int *__sample_buffer, size_t __size,
            size_t __offset, unsigned int __scale);

/*$
 * requires: valid_string(__name);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int acct (const char *__name);

// size and allocation policy not specified; using a static PATH buffer
static char _getusershell_buf[PATH_MAX];  

/*$
 * assigns: _getusershell_buf[0, PATH_MAX - 1];
 * ensures: valid_primed_substring(_getusershell_buf, PATH_MAX);
 * ensures: return == (char*)&_getusershell_buf[0];
 */
char *getusershell (void);

/*$
 * // empty contract
 */
void endusershell (void);

/*$
 * // empty contract
 */
void setusershell (void);

/*$
 * warn: "undocumented call";
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int daemon (int __nochdir, int __noclose);

#endif


#if defined __USE_MISC || (defined __USE_XOPEN && !defined __USE_XOPEN2K)

/*$
 * requires: valid_string(__path);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int chroot (const char *__path);


// NOTE: GlibC does not define PASS_MAX as password can be arbitrary long
// for now, we define it
// TODO: get rid of the constant

#ifndef PASS_MAX
#define PASS_MAX 4096
#endif


static char _getpass_buf[PASS_MAX];

/*$
 * warn: "obsolete function, do not call";
 *
 * requires: valid_string(__prompt);
 *
 * case "success" {
 *   assigns: _getpass_buf[0, PASS_MAX - 1];
 *   ensures: valid_primed_substring(_getpass_buf, PASS_MAX);
 *   ensures: return == (char*)&_getpass_buf[0];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *getpass (const char *__prompt);

#endif

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fsync (int __fd);


#ifdef __USE_GNU

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int syncfs (int __fd);

#endif


#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

/*$
 * //empty contract
 */
long int gethostid (void);

/*$
 * //empty contract
 */
void sync (void);

# if defined __USE_MISC || !defined __USE_XOPEN2K

/*$
 * warn: "not portable; use sysconf(SC_PAGESIZE) instead";
 * ensures: return >= 1;
 */
int getpagesize (void);

/*$
 * warn: "not portable; use sysconf(SC_OPEN_MAX) instead";
 * ensures: return >= 1;
 */
int getdtablesize (void);

# endif

#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

/*$
 * requires: valid_string(__file);
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int truncate (const char *__file, __off_t __length);

#endif

#if defined __USE_POSIX199309 || defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K

/*$
 * requires: __fd in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int ftruncate (int __fd, __off_t __length);

#endif


#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K) || defined __USE_MISC

/*$
 * warn: "unsupported stub; use malloc";
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int brk (void *__addr);

/*$
 * warn: "unsupported stub; use malloc";
 */
void *sbrk (intptr_t __delta);

#endif


#ifdef __USE_MISC

/*$
 * warn: "unsupported stub";
 * assigns: _errno;
 */
long int syscall (long int __sysno, ...);

#endif


#if (defined __USE_MISC || defined __USE_XOPEN_EXTENDED) && !defined F_LOCK

/*$
 * requires: __fd in FileDescriptor;
 * requires: __cmd in [0, 3];
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int lockf (int __fd, int __cmd, __off_t __len);

#endif

#ifdef __USE_GNU

/*$
 * requires: __infd in FileDescriptor;
 * requires: __outfd in FileDescriptor;
 * requires: __pinoff != NULL or size(__pinoff) >= _sizeof_off64_t;
 * requires: __poutoff != NULL or size(__poutoff) >= _sizeof_off64_t;
 *
 * case "pinoff" {
 *   assumes: __pinoff != NULL;
 *   assigns: *__pinoff;
 * }
 *
 * case "poutoff" {
 *   assumes: __poutoff != NULL;
 *   assigns: *__poutoff;
 * }
 *
 * case "success" {
 *   ensures: return in [0, __length];
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
ssize_t copy_file_range (int __infd, __off64_t *__pinoff,
			 int __outfd, __off64_t *__poutoff,
			 size_t __length, unsigned int __flags);
#endif

#if defined __USE_POSIX199309 || defined __USE_UNIX98

/*$
 * requires: __fildes in FileDescriptor;
 *
 * case "success" {
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int fdatasync (int __fildes);

#endif


#ifdef	__USE_XOPEN

/*$
 * // TODO: allocation policy is not documented, assuming dynamic allocation
 *
 * requires: valid_string(__key);
 * requires: size(__salt) >= 2;
 * 
 * case "succes" {
 *   local: char* r = new ReadOnlyMemory;
 *   ensures: size(r) == size(__key);
 *   ensures: valid_string(r);
 *   ensures: return == r;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == NULL;
 * }
 */
char *crypt (const char *__key, const char *__salt);

/*$
 * requires: __edflag in [0, 1];
 * requires: size(__glibc_block) >= 8; // en/decode 64-bit messages
 * assigns: __glibc_block[0, 7];
 * assigns: _errno;
 */
void encrypt (char *__glibc_block, int __edflag);

/*$
 * requires: size(__from) >= __n;
 * requires: size(__to) >= __n;
 *
 * case "odd" {
 *   assumes: __n >= 0 and __n & 1 == 0;
 *   assigns: __to[0, __n - 1];
 *   ensures: forall int i in [0, __n - 1]: ((char*)__from)[i] ==  (((char*)__to)[i ^ 1])';
 * }
 *
 * case "even" {
 *   assumes: __n >= 0 and __n & 1 == 1;
 *   assigns: __to[0, __n - 1];
 *   // the last byte is unspecified
 *   ensures: forall int i in [0, __n - 2]:  ((char*)__from)[i] ==  (((char*)__to)[i ^ 1])';
 * }
 *
 * case "negative" {
 *   assumes: __n < 0;
 * }
 */
void swab (const void *__restrict __from, void *__restrict __to,
           ssize_t __n);

#endif

// NOTE: ctermid and cuserid defined in stdio.c
// pthread_atfork should be defined in pthread.c

#ifdef __USE_MISC

/*$
 * requires: __length <= 256;
 * requires: size(__buffer) >= __length;
 *
 * case "success" {
 *   assigns: __buffer[0, __length - 1];
 *   ensures: return == 0;
 * }
 *
 * case "failure" {
 *   assigns: _errno;
 *   ensures: return == -1;
 * }
 */
int getentropy (void *__buffer, size_t __length);

#endif


int optind;
int opterr;
int optopt;

/*$
 * requires: forall int i in [0, size(___argv) - 1]:
 *             valid_string(___argv[i]);
 * requires: valid_string(__shortopts);
 * assigns: optind;
 * assigns: opterr;
 * assigns: optopt;
 *
 * ensures: return in [-1, 255];
 */
int getopt (int ___argc, char *const *___argv, const char *__shortopts);

