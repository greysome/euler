#include "util.h"

#define N 9999  // There are 9999 cubes < 10^12

ull cubes[N]; int I = 0;
int L[N];

int main() {
  for (int i = 1; i <= N; i++)
    cubes[I++] = (ull)i*i*i;
  printf("Largest cube is %lld\n", cubes[N-1]);

  int num_perms;
  for (int i = 0; i < N; i++) {
    num_perms = 1;
    for (int j = i+1; j < N; j++)
      if (samedigits(cubes[i], cubes[j]))
	num_perms++;

    if (num_perms == 5) {
      printf("%lld\n", cubes[i]);
      return 0;
    }

    num_perms = 0;
  }
}