// This is an exercise from The Art of Computer Programming vol 1,
// Mathematical Preliminaries
// A nice test run for my digit array implementation

#include "util.h"

int main() {
  da x = dafrom(0, 300);
  da y = dafrom(1, 300);
  for (int i = 2; i <= 1000; i++) {
    daadd(&y, x, y);
    printf("F%d = ", i);
    daprint(y);
    printf("\n");
    SWAPT(x, y, da);
  }
}