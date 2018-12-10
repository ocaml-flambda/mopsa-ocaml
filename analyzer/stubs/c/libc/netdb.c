/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


// TODO: not implemented yet

/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <netdb.h>
#include "mopsa_libc_utils.h"


static int _h_errno;

/*$
 * ensures: return == &_h_errno;
 */
int *__h_errno_location (void);

#ifdef __USE_MISC

/*$
 * case "nonull":
 *   assumes:  __str != NULL;
 *   requires: exists int i in [0, size(__str) - 1]: __str[i] == 0;
 */
void herror (const char *__str);

static const char* _hstrerror = "<placeholder string>";

/*$
 * // TODO: return a fresh string ?
 * ensures: return == _hstrerror;
 */
const char *hstrerror (int __err_num);

#endif

/*$
 * // empty contract
 */
void sethostent (int __stay_open);

/*$
 * // empty contract
 */
void endhostent (void);

static struct hostent _hostent;

/*$
 * // TODO: generate & return abstract struct hostent
 *
 * case "not_finished":
 *   assigns: _hostent;
 *   ensures: return == &_hostent;
 *
 * case "finished":
 *   ensures: return == NULL;
 */
struct hostent *gethostent (void);


// TODO: finish

struct hostent *gethostbyaddr (const void *__addr, __socklen_t __len,
                               int __type);

struct hostent *gethostbyname (const char *__name);


#ifdef __USE_MISC

struct hostent *gethostbyname2 (const char *__name, int __af);

int gethostent_r (struct hostent *__restrict __result_buf,
                  char *__restrict __buf, size_t __buflen,
                  struct hostent **__restrict __result,
                  int *__restrict __h_errnop);

int gethostbyaddr_r (const void *__restrict __addr, __socklen_t __len,
                     int __type,
                     struct hostent *__restrict __result_buf,
                     char *__restrict __buf, size_t __buflen,
                     struct hostent **__restrict __result,
                     int *__restrict __h_errnop);

int gethostbyname_r (const char *__restrict __name,
                     struct hostent *__restrict __result_buf,
                     char *__restrict __buf, size_t __buflen,
                     struct hostent **__restrict __result,
                     int *__restrict __h_errnop);

int gethostbyname2_r (const char *__restrict __name, int __af,
                      struct hostent *__restrict __result_buf,
                      char *__restrict __buf, size_t __buflen,
                      struct hostent **__restrict __result,
                      int *__restrict __h_errnop);
#endif


void setnetent (int __stay_open);

void endnetent (void);

struct netent *getnetent (void);

struct netent *getnetbyaddr (uint32_t __net, int __type);

struct netent *getnetbyname (const char *__name);

#ifdef	__USE_MISC

int getnetent_r (struct netent *__restrict __result_buf,
                 char *__restrict __buf, size_t __buflen,
                 struct netent **__restrict __result,
                 int *__restrict __h_errnop);

int getnetbyaddr_r (uint32_t __net, int __type,
                    struct netent *__restrict __result_buf,
                    char *__restrict __buf, size_t __buflen,
                    struct netent **__restrict __result,
                    int *__restrict __h_errnop);

int getnetbyname_r (const char *__restrict __name,
                    struct netent *__restrict __result_buf,
                    char *__restrict __buf, size_t __buflen,
                    struct netent **__restrict __result,
                    int *__restrict __h_errnop);

#endif

void setservent (int __stay_open);

void endservent (void);

struct servent *getservent (void);

struct servent *getservbyname (const char *__name, const char *__proto);

struct servent *getservbyport (int __port, const char *__proto);

#ifdef	__USE_MISC

int getservent_r (struct servent *__restrict __result_buf,
                  char *__restrict __buf, size_t __buflen,
                  struct servent **__restrict __result);

int getservbyname_r (const char *__restrict __name,
                     const char *__restrict __proto,
                     struct servent *__restrict __result_buf,
                     char *__restrict __buf, size_t __buflen,
                     struct servent **__restrict __result);

int getservbyport_r (int __port, const char *__restrict __proto,
                     struct servent *__restrict __result_buf,
                     char *__restrict __buf, size_t __buflen,
                     struct servent **__restrict __result);
#endif

void setprotoent (int __stay_open);

void endprotoent (void);

struct protoent *getprotoent (void);

struct protoent *getprotobyname (const char *__name);

struct protoent *getprotobynumber (int __proto);


#ifdef	__USE_MISC

int getprotoent_r (struct protoent *__restrict __result_buf,
                   char *__restrict __buf, size_t __buflen,
                   struct protoent **__restrict __result);

int getprotobyname_r (const char *__restrict __name,
                      struct protoent *__restrict __result_buf,
                      char *__restrict __buf, size_t __buflen,
                      struct protoent **__restrict __result);

int getprotobynumber_r (int __proto,
                        struct protoent *__restrict __result_buf,
                        char *__restrict __buf, size_t __buflen,
                        struct protoent **__restrict __result);

int setnetgrent (const char *__netgroup);

void endnetgrent (void);

int getnetgrent (char **__restrict __hostp,
                 char **__restrict __userp,
                 char **__restrict __domainp);

int innetgr (const char *__netgroup, const char *__host,
             const char *__user, const char *__domain);

int getnetgrent_r (char **__restrict __hostp,
                   char **__restrict __userp,
                   char **__restrict __domainp,
                   char *__restrict __buffer, size_t __buflen);

#endif


#ifdef __USE_MISC

int rcmd (char **__restrict __ahost, unsigned short int __rport,
          const char *__restrict __locuser,
          const char *__restrict __remuser,
          const char *__restrict __cmd, int *__restrict __fd2p);

int rcmd_af (char **__restrict __ahost, unsigned short int __rport,
             const char *__restrict __locuser,
             const char *__restrict __remuser,
             const char *__restrict __cmd, int *__restrict __fd2p,
             sa_family_t __af);

int rexec (char **__restrict __ahost, int __rport,
           const char *__restrict __name,
           const char *__restrict __pass,
           const char *__restrict __cmd, int *__restrict __fd2p);

int rexec_af (char **__restrict __ahost, int __rport,
              const char *__restrict __name,
              const char *__restrict __pass,
              const char *__restrict __cmd, int *__restrict __fd2p,
              sa_family_t __af);

int ruserok (const char *__rhost, int __suser,
             const char *__remuser, const char *__locuser);

int ruserok_af (const char *__rhost, int __suser,
                const char *__remuser, const char *__locuser,
                sa_family_t __af);

int iruserok (uint32_t __raddr, int __suser,
              const char *__remuser, const char *__locuser);

int iruserok_af (const void *__raddr, int __suser,
                 const char *__remuser, const char *__locuser,
                 sa_family_t __af);

int rresvport (int *__alport);

int rresvport_af (int *__alport, sa_family_t __af);

#endif


#ifdef __USE_XOPEN2K

int getaddrinfo (const char *__restrict __name,
                 const char *__restrict __service,
                 const struct addrinfo *__restrict __req,
                 struct addrinfo **__restrict __pai);

void freeaddrinfo (struct addrinfo *__ai);

const char *gai_strerror (int __ecode);

int getnameinfo (const struct sockaddr *__restrict __sa,
                 socklen_t __salen, char *__restrict __host,
                 socklen_t __hostlen, char *__restrict __serv,
                 socklen_t __servlen, int __flags);

#endif

// ommited functions in #ifdef __USE_GNU
