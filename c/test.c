#include <stdio.h>
#include <assert.h>
#include "util.h"

int main() {
  assert(maxi(10,20) == 20);
  assert(mini(10,20) == 10);

  {
  ull d; ll x, y;
  Gcd(5, 0, &d, &x, &y);
  assert(d == 5 && x == 1 && y == 0);
  Gcd(0, 5, &d, &x, &y);
  assert(d == 5 && x == 0 && y == 1);
  Gcd(23808, 447520, &d, &x, &y);
  assert(d == 32 && x == 5094 && y == -271);
  }

  {
  int log; ull x;
  Ilog(123456, 10, &log, &x);
  assert(log == 5 && x == 100000);
  }

  assert(len(5) == 1);
  assert(len(9999) == 4);
  assert(len(10000) == 5);
  assert(len(12345) == 5);

  assert(cat(0,0) == 0);
  assert(cat(0,5) == 5);
  assert(cat(5,0) == 5);
  assert(cat(123,456) == 123456);
  assert(cat(456,123) == 456123);

  assert(cat2(12,2,0) == 1200);
  assert(cat2(12,2,3) == 12003);
  assert(cat2(12,0,3) == 123);

  assert(modexp(0,5,10) == 0);
  assert(modexp(5,0,10) == 1);
  assert(modexp(2,5,7) == 4);
  assert(modexp(1234,5678,1000000) == 76096);

  assert(sumdigits(0) == 0);
  assert(sumdigits(5) == 5);
  assert(sumdigits(123456) == 21);

  assert(iexp(5,0) == 1);
  assert(iexp(2,5) == 32);

  ull root; bool is_power;
  Iroot(4, 2, &root, &is_power);
  assert(root == 2 && is_power);
  Iroot(28, 3, &root, &is_power);
  assert(root == 3 && !is_power);

  assert(!ispolygonal(0,1));
  for (int n = 1; n < 100; n++) {
    assert(ispolygonal(n*(n+1)/2, 3));
    assert(ispolygonal(n*n, 4));
    assert(ispolygonal(n*(3*n-1)/2, 5));
    assert(ispolygonal(n*(2*n-1), 6));
    assert(ispolygonal(n*(5*n-3)/2, 7));
  }
  assert(!ispolygonal(11,3));
  assert(!ispolygonal(17,4));
  assert(!ispolygonal(23,5));

  assert(isprime(2));
  assert(!isprime(4));
  assert(isprime(11));
  assert(isprime(3111296389));
  assert(!isprime(3111296388));

  for (ull n = 100000000; n < 100020000; n += 3) {
    bool is_prime;
    ull d = getfactor(n, &is_prime);
    assert(isprime(n) == is_prime);
    assert(!is_prime || d == n);
    assert(n % d == 0);
  }

  assert(phi(1) == 1);
  assert(phi(2) == 1);
  assert(phi(3) == 2);
  assert(phi(4) == 2);
  assert(phi(9) == 6);
  assert(phi(15) == 8);

  int L[4] = {1,2,3,4};
  nextperm(L,4);
  assert(L[0] == 1 && L[1] == 2 && L[2] == 4 && L[3] == 3);

  nextperm(L,4);
  assert(L[0] == 1 && L[1] == 3 && L[2] == 2 && L[3] == 4);
  nextperm(L,4);
  assert(L[0] == 1 && L[1] == 3 && L[2] == 4 && L[3] == 2);
  for (int i = 0; i < 24-4; i++)
    nextperm(L,4);
  assert(L[0] == 4 && L[1] == 3 && L[2] == 2 && L[3] == 1);
  assert(!nextperm(L,4));

  L[0] = 1; L[1] = 2; L[2] = 3; L[3] = 4;
  reverse(L, 4);
  assert(L[0] == 4 && L[1] == 3 && L[2] == 2 && L[3] == 1);

  initrng();
  L[0] = 1; L[1] = 2; L[2] = 3; L[3] = 4;
  shuffle(L, 4);
  printf("after shuffling 1 2 3 4: %d %d %d %d\n", L[0], L[1], L[2], L[3]);

  int A[4] = {1,2,3,4};
  int B[4] = {4,3,2,1};
  int C[4] = {1,2,3,5};
  assert(sameelements(A, B, 4));
  assert(!sameelements(A, C, 4));

  char race[] = "RACE";
  char care[] = "CARE";
  char aa[] = "AA";
  char aaaaaaa[] = "AAAAAAA";
  char what[] = "WHAT";
  char _long[] = "LONG";
  assert(anagrams(race, care, 4));
  assert(anagrams(race, care, 10));
  assert(!anagrams(aa, aaaaaaa, 10));
  assert(!anagrams(what, _long, 4));
  assert(!anagrams(what, _long, 10));

  int D[5];
  assert(digits(12345, D) == 5);
  assert(D[0] == 1 && D[1] == 2 && D[2] == 3 && D[3] == 4 && D[4] == 5);

  assert(digits(8100, D) == 4);
  assert(D[0] == 8 && D[1] == 1 && D[2] == 0 && D[3] == 0);

  assert(samedigits(41063625, 56623104));
  assert(samedigits(1006, 1060));
  assert(!samedigits(100, 1000));

  int dists_from_0[9] = {-1, +4, -1, -1, -1, -1, -1, +8, -1};
  int dists_from_1[9] = {+4, -1, +8, -1, -1, -1, -1, +11, -1};
  int dists_from_2[9] = {-1, +8, -1, +7, -1, +4, -1, -1, +2};
  int dists_from_3[9] = {-1, -1, +7, -1, +9, +14, -1, -1, -1};
  int dists_from_4[9] = {-1, -1, -1, +9, -1, +10, -1, -1, -1};
  int dists_from_5[9] = {-1, -1, +4, +14, +10, -1, +2, -1, -1};
  int dists_from_6[9] = {-1, -1, -1, -1, -1, +2, -1, +1, +6};
  int dists_from_7[9] = {+8, +11, -1, -1, -1, -1, +1, -1, +7};
  int dists_from_8[9] = {-1, -1, +2, -1, -1, -1, +6, +7, -1};
  int *dists[9] = {dists_from_0, dists_from_1, dists_from_2,
		   dists_from_3, dists_from_4, dists_from_5,
		   dists_from_6, dists_from_7, dists_from_8};
  assert(dijkstra(dists, 0, 8, 9) == 14);

  printf("All tests passed!\n");
}