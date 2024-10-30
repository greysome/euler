// Area of a-c-c triangle is equal to a sqrt(c^2-a^2), which is only
// integral if a,c are part of a Pythagorean triple (a,b,c) or
// (b,a,c).
//
// Furthermore, triple must be primitive (otherwise 2*a and c differ
// by more than 1). Primitive triples are uniquely parametrised by m,n
// where m>n are coprime and both have different parities:
// a=m^2-n^2, b=2mn, c=m^2+n^2.
//
// Since c < (10^9+1) / 3, it suffices to generate triples where
// m < sqrt((10^9+1)/3) approx 20000.

#include "util.h"

#define N 1000000000

int main() {
  ull total = 0;
  int k = isqrt((N+1)/3);
  for (int m = 1; m <= k; m++) {
    printf("%d %d %lld\n", m, k, total);
    for (int n = 1; n < m; n++) {
      if (m%2 == n%2) continue;
      if (gcd(m,n) > 1) continue;
      int a = m*m - n*n;
      int b = 2*m*n;
      int c = m*m + n*n;
      if (2*a + 1 == c || 2*a - 1 == c) total += 2*a + 2*c;
      if (2*b + 1 == c || 2*b - 1 == c) total += 2*b + 2*c;
    }
  }
  printf("%lld\n", total);
}