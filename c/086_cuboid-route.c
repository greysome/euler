// Brute force method. It's slightly slow but it gets the job done

#include "util.h"

int min3(int x, int y, int z) {
  return x < y ? mini(x, z) : mini(y, z);
}

#define THRESHOLD 1000000
int main() {
  int count = 0;
  for (int i = 1; i <= THRESHOLD; i++) {
    for (int j = 1; j <= i; j++) {
      for (int k = 1; k <= j; k++) {
	int len1 = i*i + (j+k)*(j+k);
	int len2 = j*j + (i+k)*(i+k);
	int len3 = k*k + (i+j)*(i+j);
	int shortest = min3(len1, len2, len3);
	// Guaranteed not to be a square
	if (shortest % 4 == 2 || shortest % 4 == 3)
	  continue;

	int isqrt = iroot(shortest, 2);
	if (isqrt*isqrt == shortest) {
	  count++;
	  if (count % 1000 == 0)
	    printf("count=%d i=%d\n", count, i);
	}
	if (count == THRESHOLD) {
	  printf("%d\n", i);
	  return 0;
	}
      }
    }
  }
}