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
#include <netinet/in.h>
#include "mopsa_libc_utils.h"


/*$
 * // empty contract
 * // we could be more precise
 */
uint32_t ntohl (uint32_t __netlong);

/*$
 * // empty contract
 * // we could be more precise
 */
uint16_t ntohs (uint16_t __netshort);

/*$
 * // empty contract
 * // we could be more precise
 */
uint32_t htonl (uint32_t __hostlong);

/*$
 * // empty contract
 * // we could be more precise
 */
uint16_t htons (uint16_t __hostshort);


#ifdef __USE_MISC

/*$
 * requires:  __sockfd in FileDescriptor;
 *
 * case "assign":
 *   assumes: __sock_in != _NULL;
 *   assigns: *__sock_in;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int bindresvport (int __sockfd, struct sockaddr_in *__sock_in);

/*$
 * //TODO: undocumented
 * requires:  __sockfd in FileDescriptor;
 *
 * case "assign":
 *   assumes: __sock_in != _NULL;
 *   assigns: *__sock_in;
 *
 * case "success":
 *   ensures: return == 0;
 *
 * case "failure":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int bindresvport6 (int __sockfd, struct sockaddr_in6 *__sock_in);

#endif


/* TODO: undocumented functions, included with__USE_GNU

#ifdef __USE_GNU
int inet6_option_space (int __nbytes);
int inet6_option_init (void *__bp, struct cmsghdr **__cmsgp, int __type);
int inet6_option_append (struct cmsghdr *__cmsg, const uint8_t *__typep, int __multx, int __plusy);
uint8_t *inet6_option_alloc (struct cmsghdr *__cmsg, int __datalen, int __multx, int __plusy);
int inet6_option_next (const struct cmsghdr *__cmsg, uint8_t **__tptrp);
int inet6_option_find (const struct cmsghdr *__cmsg, uint8_t **__tptrp, int __type);
int inet6_opt_init (void *__extbuf, socklen_t __extlen) ;
int inet6_opt_append (void *__extbuf, socklen_t __extlen, int __offset, uint8_t __type, socklen_t __len, uint8_t __align, void **__databufp);
int inet6_opt_finish (void *__extbuf, socklen_t __extlen, int __offset);
int inet6_opt_set_val (void *__databuf, int __offset, void *__val, socklen_t __vallen);
int inet6_opt_next (void *__extbuf, socklen_t __extlen, int __offset, uint8_t *__typep, socklen_t *__lenp, void **__databufp);
int inet6_opt_find (void *__extbuf, socklen_t __extlen, int __offset, uint8_t __type, socklen_t *__lenp,  void **__databufp);
int inet6_opt_get_val (void *__databuf, int __offset, void *__val, socklen_t __vallen);

socklen_t inet6_rth_space (int __type, int __segments);
void *inet6_rth_init (void *__bp, socklen_t __bp_len, int __type, int __segments);
int inet6_rth_add (void *__bp, const struct in6_addr *__addr);
int inet6_rth_reverse (const void *__in, void *__out);
int inet6_rth_segments (const void *__bp);
struct in6_addr *inet6_rth_getaddr (const void *__bp, int __index);
int getipv4sourcefilter (int __s, struct in_addr __interface_addr, struct in_addr __group, uint32_t *__fmode, uint32_t *__numsrc, struct in_addr *__slist);
int setipv4sourcefilter (int __s, struct in_addr __interface_addr, struct in_addr __group, uint32_t __fmode, uint32_t __numsrc, const struct in_addr *__slist);
int getsourcefilter (int __s, uint32_t __interface_addr, const struct sockaddr *__group, socklen_t __grouplen, uint32_t *__fmode, uint32_t *__numsrc, struct sockaddr_storage *__slist);
int setsourcefilter (int __s, uint32_t __interface_addr, const struct sockaddr *__group, socklen_t __grouplen, uint32_t __fmode, uint32_t __numsrc, const struct sockaddr_storage *__slist);
#endif

*/
