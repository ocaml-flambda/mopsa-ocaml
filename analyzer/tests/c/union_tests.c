#include "mopsa.h"

typedef unsigned char uint8;
typedef unsigned short uint16;

union {
  struct {uint8 al, ah, bl, bh;} b;
  struct {uint16 ax, bx;} w;
} regs;

void test_initialization_of_uninitialized_global() {
  _mopsa_assert(regs.w.ax == 0);
  _mopsa_assert(regs.b.al == 0);
}

void test_modify_part_and_reconstruct() {
  regs.w.ax = 0x1234;
  if (!regs.b.ah) regs.b.bl = regs.b.al;
  else regs.b.bh = regs.b.al;
  regs.b.al = 0xab;
  _mopsa_assert(regs.w.ax == 0x12ab);
}
