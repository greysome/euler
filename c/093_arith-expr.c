#include "util.h"

// Maximum expressible value is 6*7*8*9 = 3024
int expressible[3024] = {0};
int max = 0; int argmax[4];

#define EPS 0.0001

float f(float x, int op, float y) {
  if (op==3 && y==0.0f) return INFINITY;
  switch (op) {
  case 0: return x+y;
  case 1: return x-y;
  case 2: return x*y;
  case 3: return x/y;
  }
}

char *opchars = "+-*/";

void handle(int a, int b, int c, int d, int op1, int op2, int op3) {
  int perm[4] = {a,b,c,d};

  do {
    a = perm[0];
    b = perm[1];
    c = perm[2];
    d = perm[3];
    float x1 = f(f(a,op1,b), op2, f(c,op3,d));
    float x2 = f(a, op1, f(f(b,op2,c), op3, d));
    float x3 = f(a, op1, f(b, op2, f(c,op3,d)));
    float x4 = f(f(f(a,op1,b), op2, c), op3, d);
    float x5 = f(f(a, op1, f(b,op2,c)), op3, d);

#ifdef DBG
    printf("(%d%c%d)%c(%d%c%d) = %f\n", a, opchars[op1], b, opchars[op2], c, opchars[op3], d, x1);
    printf("%d%c((%d%c%d)%c%d) = %f\n", a, opchars[op1], b, opchars[op2], c, opchars[op3], d, x2);
    printf("%d%c(%d%c(%d%c%d)) = %f\n", a, opchars[op1], b, opchars[op2], c, opchars[op3], d, x3);
    printf("((%d%c%d)%c%d)%c%d = %f\n", a, opchars[op1], b, opchars[op2], c, opchars[op3], d, x4);
    printf("(%d%c(%d%c%d))%c%d = %f\n", a, opchars[op1], b, opchars[op2], c, opchars[op3], d, x5);
#endif

    int i1 = roundf(x1);
    int i2 = roundf(x2);
    int i3 = roundf(x3);
    int i4 = roundf(x4);
    int i5 = roundf(x5);

    if (x1 != INFINITY && fabsf(x1-i1) <= EPS && i1 >= 1) expressible[i1-1] = 1;
    if (x2 != INFINITY && fabsf(x2-i2) <= EPS && i2 >= 1) expressible[i2-1] = 1;
    if (x3 != INFINITY && fabsf(x3-i3) <= EPS && i3 >= 1) expressible[i3-1] = 1;
    if (x4 != INFINITY && fabsf(x4-i4) <= EPS && i4 >= 1) expressible[i4-1] = 1;
    if (x5 != INFINITY && fabsf(x5-i5) <= EPS && i5 >= 1) expressible[i5-1] = 1;
  } while (nextperm(perm, 4));
}

int main() {
  for (int a=0; a<=9; a++) {
    for (int b=a+1; b<=9; b++)  {
      for (int c=b+1; c<=9; c++) {
	for (int d=c+1; d<=9; d++) {
	  memset(expressible, 0, 3024*sizeof(int));
	  // Try all possible combinations
	  for (int op1=0; op1<=3; op1++) {
	    for (int op2=0; op2<=3; op2++) {
	      for (int op3=0; op3<=3; op3++) {
		handle(a,b,c,d,op1,op2,op3);
	      }
	    }
	  }

	  int n;
	  for (n = 0; n < 3024; n++) {
	    if (!expressible[n])
	      break;
	  }
	  if (n > max) {
	    max = n;
	    argmax[0] = a;
	    argmax[1] = b;
	    argmax[2] = c;
	    argmax[3] = d;
	  }
	}
      }
    }
  }

  printf("%d %d %d %d %d\n", max, argmax[0], argmax[1], argmax[2], argmax[3]);
}