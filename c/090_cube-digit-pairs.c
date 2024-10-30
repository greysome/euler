// Just brute force through all possible combinations lol
// There are at most (10 choose 6)^2 approx 40000 of them

#include "util.h"

int compare(int *xs, int *ys, int n) {
  for (int i = 0; i < n; i++) {
    if (xs[i] < ys[i])
      return -1;
    else if (xs[i] > ys[i])
      return 1;
  }
  return 0;
}

// Can the 2-digit number ab be expressed with the cubes with faces xs
// and ys?
bool expressible(int a, int b, int *xs, int *ys, int n) {
  return (contains(xs,a,n) && contains(ys,b,n)) ||
    (contains(xs,b,n) && contains(ys,a,n));
}

int main() {
  int count = 0;
  for (int a1=0; a1<=9; a1++) {
  for (int a2=a1+1; a2<=9; a2++) {
  for (int a3=a2+1; a3<=9; a3++) {
  for (int a4=a3+1; a4<=9; a4++) {
  for (int a5=a4+1; a5<=9; a5++) {
  for (int a6=a5+1; a6<=9; a6++) {
    for (int b1=0; b1<=9; b1++) {
    for (int b2=b1+1; b2<=9; b2++) {
    for (int b3=b2+1; b3<=9; b3++) {
    for (int b4=b3+1; b4<=9; b4++) {
    for (int b5=b4+1; b5<=9; b5++) {
    for (int b6=b5+1; b6<=9; b6++) {
      int as[6] = {a1,a2,a3,a4,a5,a6};
      int bs[6] = {b1,b2,b3,b4,b5,b6};
      if (compare(as,bs,6) == 1) continue;

#define CAN(a,b) expressible(a,b,as,bs,6)
      if (CAN(0,1) &&
	  CAN(0,4) &&
	  (CAN(0,9) || CAN(0,6)) &&
	  (CAN(1,6) || CAN(1,9)) &&
	  CAN(2,5) &&
	  (CAN(3,6) || CAN(3,9)) &&
	  (CAN(4,9) || CAN(4,6)) &&
	  (CAN(6,4) || CAN(9,4)) &&
	  CAN(8,1)) {
	count++;
      }
    }
    }
    }
    }
    }
    }
  }
  }
  }
  }
  }
  }
  printf("%d\n", count);
}