#include "util.h"

void test(int a, int b, int c, int d, int e, int f) {
  for (int i = 10; i < 100; i++) {
    for (int j = 10; j < 100; j++) {
      int A = cat(i,j);
      if (!ispolygonal(A,a)) continue;

      for (int k = 10; k < 100; k++) {
	int B = cat(j,k);
	if (!ispolygonal(B,b)) continue;

	for (int l = 10; l < 100; l++) {
	  int C = cat(k,l);
	  if (!ispolygonal(C,c)) continue;

	  for (int m = 10; m < 100; m++) {
	    int D = cat(l,m);
	    if (!ispolygonal(D,d)) continue;

	    for (int n = 10; n < 100; n++) {
	      int E = cat(m,n);
	      if (!ispolygonal(E,e)) continue;

	      int F = cat(n,i);
	      if (!ispolygonal(F,f)) continue;
	      printf("%d %d %d %d %d %d %d\n", A, B, C, D, E, F, A+B+C+D+E+F);
	      return;
	    }
	  }
	}
      }
    }
  }
}

int main() {
  int L[6] = {3,4,5,6,7,8};
  int i = 0;
  while (true) {
    i++; printf("%d permutations tested\n", i);
    test(L[0],L[1],L[2],L[3],L[4],L[5]);
    if (!nextperm(L,6)) break;
  }
}