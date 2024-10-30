// SOLUTION OUTLINE:
// I generate primitive Pythagorean triplets via the parametrisation
// (u^2-v^2, 2uv, u^2+v^2),
// where u>v>0 and (u,v)=1.
//
// These are stored in the array `primsols` as follows:
// Each index corresponds to a sum L. By mod 2 arguments L is
// even. Each entry consists of 4 numbers: the first number indicates
// whether there is no *primitive* solution (a,b,c) with a+b+c=L, a
// unique primitive solution, or multiple primitive solutions. The
// next 3 numbers give the unique solution if it exists.
//
// We then populate array `actlsols` which works the same way as
// `primsols`, but we also account for non-primitive solutions
// (a,b,c). Thus by counting the number of entries of `actlsols` with
// unique (primitive or non-primitive) solutions, we obtain the final
// answers.

#include "util.h"

int n = 1500000;
int primsols[1500000/2+1][4];
int actlsols[1500000/2+1][4];

#define NO_SOL 0
#define UNIQUE_SOL 1
#define MULTIPLE_SOLS 2

int main() {
  int r = iroot(n, 2);

  // Initialize arrays
  for (int i = 0; i <= n/2; i++) {
    for (int j = 0; j < 4; j++) {
      primsols[i][j] = 0;
      actlsols[i][j] = 0;
    }
  }

  // 1. Populate primitive solutions
  for (int u = 1; u <= r; u++) {
    for (int v = 1; v < u; v++) {
      if (gcd(u,v) != 1) continue;
      int c = u*u + v*v;
      // The pythagorean triple (a,b,c)
      int a = u*u - v*v;
      int b = 2*u*v;

      int L = a+b+c;
      if (L > n) continue;

      if (primsols[L/2][0] == UNIQUE_SOL)
	primsols[L/2][0] = MULTIPLE_SOLS;

      if (primsols[L/2][0] == NO_SOL) {
	if (a > b) {
	  int tmp = b; b = a; a = tmp;
	}
	primsols[L/2][0] = UNIQUE_SOL;
	primsols[L/2][1] = a;
	primsols[L/2][2] = b;
	primsols[L/2][3] = c;
      }
    }
  }

  // 2. Extend to all solutions
  for (int i = 1; i <= n/2; i++) {
    int x = primsols[i][0];
    if (x != UNIQUE_SOL) continue;
    int a = primsols[i][1];
    int b = primsols[i][2];
    int c = primsols[i][3];

    for (int j = i; j <= n/2; j += i) {
      int x_ = actlsols[j][0];
      if (x_ == NO_SOL) {
	// Populate the scalar multiple of the primitive solution
	// (a,b,c), if the multiple of the original sum has no
	// primitive solutions.
	actlsols[j][0] = UNIQUE_SOL;
	actlsols[j][1] = j/i*a;
	actlsols[j][2] = j/i*b;
	actlsols[j][3] = j/i*c;
      }
      else if (x_ == UNIQUE_SOL) {
	int a_ = actlsols[j][1];
	int b_ = actlsols[j][2];
	int c_ = actlsols[j][3];
	// If any existing solution (a_,b_,c_) in the multiple of the
	// original sum is not a scalar multiple of the given
	// primitive solution (a,b,c), then we have multiple solutions
	// overall.
	if (j/i*a != a_ || j/i*b != b_ || j/i*c != c_)
	  actlsols[j][0] = MULTIPLE_SOLS;
      }
    }
  }

  // Count solutions
  int count = 0;
  for (int i = 0; i <= n/2; i++) {
    int x = actlsols[i][0];
    if (x == UNIQUE_SOL)
      count++;
  }
  printf("%d\n", count);
}