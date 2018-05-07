#include<stdio.h>
#include "mopsa.h"
#include "stddef.h"
#include "sys/socket.h"

char* fgets(char *str, int n, FILE *stream) {
  for (int i = 0; i < n ; i++ ) {
    str[i] = _mopsa_range_char();
  };
  if (_mopsa_rand_int(0,1)) {
    return str;
  } else {
    return NULL;
  }
}

int atoi(const char *str) {
  return _mopsa_range_int();
}

void *memset(void *s, int c, size_t n) {
  size_t i;
  for (i = 0 ; i < n ; i++) {
    ((char*) s)[i] = _mopsa_range_char();
  };
  return s;

}

int socket(int domain, int type, int protocol) {
  return _mopsa_range_int();
}

int connect (int fd, __CONST_SOCKADDR_ARG addr, socklen_t len) {
  return _mopsa_range_int();
}

unsigned short htons(unsigned int u) {
  return _mopsa_range_unsigned_short();
}

int close(int fd) {
  return _mopsa_range_int();
}

ssize_t recv(int socket, void *buffer, size_t length, int flags) {
  size_t i;
  for (i = 0 ; i < length ; i++) {
    ((char*) buffer)[i] = _mopsa_range_char();
  };
  return _mopsa_rand_int(-1,length);
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
  return _mopsa_range_int();
}

int listen(int socket, int backlog) {
  return _mopsa_range_int();
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
  if (addr != NULL) {
    int i;
    addr->sa_family = _mopsa_range_unsigned_short();
    for (i = 0 ; i < 14 ; i ++) {
      addr->sa_data[i] = _mopsa_range_char();
    };
    *addrlen = _mopsa_range_long();
  };
  return _mopsa_range_int();
}
