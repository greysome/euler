// Just brute force it lol, because the square sum digit of a large
// number will always be relatively small (mostly < 100 say)

#include "util.h"

#define N 10000000

int arr[7];

int main() {
  int count = 0;
  for (int k = 1; k < N; k++) {
    int n = k;
    while (true) {
      int numdigits = digits(n, arr);
      int sum = 0;
      for (int i = 0; i < numdigits; i++)
	sum += arr[i]*arr[i];
      if (sum == 1) break;
      if (sum == 89) {
	count += 1;
	break;
      }
      n = sum;
    }
  }
  printf("%d\n", count);
}