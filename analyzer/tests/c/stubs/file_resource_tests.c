#include <fcntl.h>

extern void *_mopsa_int_to_fd(int fd);


/*$
 * local: int fd = new FileDescriptor;
 * ensures: return == fd;
 */
int open_(const char *file, int oflag);


/*$
 * requires : fd in FileDescriptor;
 * local : void * addr = _mopsa_int_to_fd(fd);
 * free : addr;
 */
void close_(int fd);


/* Test that open returns a positive number */
void test_open_returns_positive() {
  int fd = open_("/tmp/a.txt", O_RDONLY);
  _mopsa_assert_safe();
  _mopsa_assert(fd >= 0);
}


/* Test that open returns increasing numbers */
void test_open_returns_increasing_ids() {
  int fd1 = open_("/tmp/a.txt", O_RDONLY);
  int fd2 = open_("/tmp/b.txt", O_RDONLY);
  _mopsa_assert(fd2 > fd1);
}


/* Test closing a file after opening it */
void test_close_after_open() {
  int fd = open_("/tmp/a.txt", O_RDONLY);
  close_(fd);
  _mopsa_assert_safe();
}

/* Test closing stdin */
void test_close_stdin() {
  close_(0);
  _mopsa_assert_safe();
}


/* Test redirecting a file descriptor */
void test_redirect_stdin() {
  close_(0);
  int fd = open_("/tmp/b.txt", O_RDONLY);
  _mopsa_assert(fd == 0);
}

void test_open_returns_the_smallest_id() {
  int fd1 = open_("/tmp/a.txt", O_RDONLY);
  close_(fd1);
  int fd2 = open_("/tmp/b.txt", O_RDONLY);
  _mopsa_assert(fd1 == fd2);
}

/* Test closing a file not already opened */
void test_close_without_open() {
  int fd = 20;
  close_(fd);
  _mopsa_assert_unsafe();
}


/* Test closing a file already closes */
void test_close_after_close() {
  int fd = open_("/tmp/a.txt", O_RDONLY);
  close_(fd);
  _mopsa_assert_safe();

  close_(fd);
  _mopsa_assert_unsafe();
}
