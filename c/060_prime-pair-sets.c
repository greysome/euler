// SOLUTION OUTLINE:
// Define x ~ y if the concatenations xy and yx are both prime.
// Define S be the set of primes bounded by a sufficiently large
// number N.
//
// I build an adjacency matrix M representing ~ on S, and carry out a
// 5-fold loop to find primes p1,...,p5 such that pi ~ pj for all i!=j
//
// A crucial optimization: with the exception of 3, we have x ~ y only
// if the digital sums of x and y are equal mod 3 (i.e. both 1 or both
// 2). Otherwise xy and yx are divisible by 3.
//
// Thus I actually compute two sets of primes S1, S2 corresponding to
// the two possible residues, as well as two matrices M1, M2. Then I
// do the 5-fold loop within M1 and M2 separately.

#include "util.h"

#define N 1000
ull primes1[N]; char M1[N][N];
ull primes2[N]; char M2[N][N];

int main() {
  primes1[0] = 3; int I = 1;
  primes2[0] = 3; int J = 1;
  ull x = 3;
  while (true) {
    x += 2;
    if (!isprime(x)) continue;

    int sum_mod = sumdigits(x) % 3;
    if (sum_mod == 1 && I < N)
      primes1[I++] = x;
    else if (sum_mod == 2 && J < N)
      primes2[J++] = x;

    if (I >= N && J >= N) break;
  }
  printf("Generated primes, largest=%lld %lld\n", primes1[N-1], primes2[N-1]);

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      M1[i][j] = 0;
      M2[i][j] = 0;
    }
  }

  for (int i = 0; i < N; i++) {
    for (int j = i+1; j < N; j++) {
      ull p = primes1[i], q = primes1[j];
      if (isprime(cat(p,q)) && isprime(cat(q,p))) {
	M1[i][j] = 1;
	M1[j][i] = 1;
      }
      p = primes2[i]; q = primes2[j];
      if (isprime(cat(p,q)) && isprime(cat(q,p))) {
	M2[i][j] = 1;
	M2[j][i] = 1;
      }
    }
  }

  for (int i = 0; i < N; i++) {
    for (int j = i+1; j < N; j++) {
      if (!M1[i][j]) continue;
      for (int k = j+1; k < N; k++) {
	if (!M1[i][k] || !M1[j][k]) continue;
	for (int l = k+1; l < N; l++) {
	  if (!M1[i][l] || !M1[j][l] || !M1[k][l]) continue;
	  for (int m = l+1; m < N; m++) {
	    if (!M1[i][m] || !M1[j][m] || !M1[k][m] || !M1[l][m]) continue;
	    ull p = primes1[i], q = primes1[j], r = primes1[k], s = primes1[l], t = primes1[m];
	    printf("%lld %lld %lld %lld %lld %lld\n", p,q,r,s,t, p+q+r+s+t);
	  }
	}
      }
    }
  }
  printf("Searched all primes with digit sum = 1 mod 3\n");

  for (int i = 0; i < N; i++) {
    for (int j = i+1; j < N; j++) {
      if (!M2[i][j]) continue;
      for (int k = j+1; k < N; k++) {
	if (!M2[i][k] || !M2[j][k]) continue;
	for (int l = k+1; l < N; l++) {
	  if (!M2[i][l] || !M2[j][l] || !M2[k][l]) continue;
	  for (int m = l+1; m < N; m++) {
	    if (!M2[i][m] || !M2[j][m] || !M2[k][m] || !M2[l][m]) continue;
	    ull p = primes2[i], q = primes2[j], r = primes2[k], s = primes2[l], t = primes2[m];
	    printf("%lld %lld %lld %lld %lld %lld\n", p,q,r,s,t, p+q+r+s+t);
	  }
	}
      }
    }
  }
  printf("Searched all primes with digit sum = 2 mod 3\n");
}