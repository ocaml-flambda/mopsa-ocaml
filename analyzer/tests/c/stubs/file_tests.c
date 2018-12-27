#include <fcntl.h>

extern int _mopsa_fd_to_int(void *fd);
extern void *_mopsa_int_to_fd(int fd);

/*$
 * local: void* fd = new FileDescriptor;
 * local: int n = _mopsa_fd_to_int(fd);
 * ensures: return == n;
 */
int open_ (const char *file, int oflag, ...);

void test_open() {
  int fd = open_("/tmp/a.txt", O_RDONLY);
  _mopsa_assert(fd >= 0);
}
