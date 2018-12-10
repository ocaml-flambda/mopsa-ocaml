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
#include <arpa/inet.h>
#include "mopsa_libc_utils.h"

static const int _AF_INET = AF_INET;
static const int _AF_INET6 = AF_INET6;
static const int _INET_ADDRSTRLEN = INET_ADDRSTRLEN;
static const int _INET6_ADDRSTRLEN = INET6_ADDRSTRLEN;


// defined in netinet/in.c: ntohl, ntohs, htonl, htons

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 */
in_addr_t inet_addr (const char *__cp);

/*$
 * // empty contract
 */
in_addr_t inet_lnaof (struct in_addr __in);

/*$
 * // empty contract
 */
struct in_addr inet_makeaddr (in_addr_t __net, in_addr_t __host);

/*$
 * // empty contract
 */
in_addr_t inet_netof (struct in_addr __in);

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 */
in_addr_t inet_network (const char *__cp);


static char _inet_ntoa_buf[16]; // room for an IPv4 number-and-dots address

/*$
 * assigns: _inet_ntoa_buf[0, size(_inet_ntoa_buf) - 1];
 * ensures: return == &(_inet_ntoa_buf[0]) and 
 *          exists int i in [0, size(_inet_ntoa_buf) - 1]: _inet_ntoa_buf[i] == 0;
 */
char *inet_ntoa (struct in_addr __in);

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 * requires: __af == _AF_INET or __af == _AF_INET6;
 *
 * case "IPv4":
 *   assumes:  __af == _AF_INET;
 *   requires: size(__buf) >= 4;
 *   assigns:  __buf[0, 3];
 *   ensures:  return == 1;
 *
 * case "IPv6":
 *   assumes:  __af == _AF_INET6;
 *   requires: size(__buf) >= 16;
 *   assigns:  __buf[0, 15];
 *   ensures:  return == 1;
 *
 * case "invalid address":
 *   ensures: return == 0; 
 *
 * // the requires on __af prevents the case return == -1 (invalid 
 * // address family) to occur
 */
int inet_pton (int __af,
               const char *__restrict __cp,
               void *__restrict __buf);

/*$
 * requires: __af == _AF_INET or __af == _AF_INET6;
 * requires: size(__buf) >= __len;
 *
 * case "IPv4":
 *   assumes:  __af == _AF_INET;
 *   requires: size(__cp) >= 4;
 *   requires: __len >= _INET_ADDRSTRLEN;
 *   assigns: __buf[0, __len - 1];
 *   ensures: exists int i in [0, __len - 1]: __buf[i] == 0;
 *   ensures:  return == __buf;
 *
 * case "IPv6":
 *   assumes:  __af == _AF_INET6;
 *   requires: size(__cp) >= 16;
 *   requires: __len >= _INET6_ADDRSTRLEN;
 *   assigns: __buf[0, __len - 1];
 *   ensures: exists int i in [0, __len - 1]: __buf[i] == 0;
 *   ensures:  return == __buf;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == _NULL; 
 */
const char *inet_ntop (int __af,
                       const void *__restrict __cp,
                       char *__restrict __buf,
                       socklen_t __len);


#ifdef __USE_MISC

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 * assigns:  *__inp;
 * ensures:  return == 0 || return == 1;
 */
int inet_aton (const char *__cp, struct in_addr *__inp);

/*$
 * requires: size(__buf) >= __len;
 *
 * case "success":
 *   assigns:  __buf[0, __len - 1];
 *   ensures:  exists int i in [0, __len - 1]: __buf[i] == 0;
 *   ensures:  return == __buf;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
char *inet_neta (in_addr_t __net, char *__buf, size_t __len);

/*$
 * requires: 8 * size(__cp) >= __bits;
 * requires: __af == _AF_INET;
 *
 * case "success":
 *   assigns:  __buf[0, __len - 1];
 *   ensures:  exists int i in [0, __len - 1]: __buf[i] == 0;
 *   ensures:  return == __buf;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == _NULL;
 */
char *inet_net_ntop (int __af, const void *__cp, int __bits,
                     char *__buf, size_t __len);

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 * requires: __af == _AF_INET;
 * requires: size(__buf) >= __len;
 *
 * case "success":
 *   assigns:  __buf[0, return - 1];
 *   ensures:  return >= 0 and return <= __len;
 *
 * case "error":
 *   assigns: _errno;
 *   ensures: return == -1;
 */
int inet_net_pton (int __af, const char *__cp,
                   void *__buf, size_t __len);

/*$
 * requires: exists int i in [0, size(__cp) - 1]: __cp[i] == 0;
 * requires: size(__buf) >= __len;
 * assigns:  __buf[0, return - 1];
 * ensures:  return <= __len;
 * //warn: "undocumented function"
 */
unsigned int inet_nsap_addr (const char *__cp,
                             unsigned char *__buf, int __len);

/*$
 * requires: size(__buf) >= 3 * __len;
 * requires: size(__cp) >= __len;
 * assigns:  __buf[0, 3 * __len - 1];
 * ensures:  exists int i in [0, 3 * __len - 1]: __buf[i] == 0;
 * ensures:  return == __buf;
 * //warn: "undocumented function"
 */
char *inet_nsap_ntoa (int __len, const unsigned char *__cp, char *__buf);

#endif
