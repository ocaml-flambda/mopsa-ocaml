#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main() {
  int fd = open("/dev/null", O_RDWR);
  if (fd < 0) {
    return -1;
  }
  dup2(fd, fileno(stdout));   // redirect stdout to /dev/null
  close(fd);                  // fd no longer needed since it is dup'ed
  printf("Hello\n");          // written to /dev/null
}
