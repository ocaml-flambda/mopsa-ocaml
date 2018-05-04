#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H

#include<sys/types.h>
struct sockaddr {
    unsigned short    sa_family;    // address family, AF_xxx
    char              sa_data[14];  // 14 bytes of protocol address
};

int socket(int domain, int type, int protocol);
#define __CONST_SOCKADDR_ARG	const struct sockaddr *

int connect (int fd, __CONST_SOCKADDR_ARG addr, socklen_t len);

unsigned short htons(unsigned int);

int close(int fd);

ssize_t recv(int socket, void *buffer, size_t length, int flags);

#define AF_INET 2
#define SOCK_STREAM 1

#endif
