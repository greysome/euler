#include "util.h"

#define MAXK 12000
//#define MYDEBUG

int mins[MAXK+1] = {0};
int X[14];

void prodsum(int *_prod, int *_sum, int n) {
  assert(n <= 15);
  int prod = 1, sum = 0;
  for (int i = 0; i < n; i++) {
    prod *= X[i]; sum += X[i];
  }
  if (_prod != NULL) *_prod = prod;
  if (_sum != NULL) *_sum = sum;
}

void handle_case2(int k) {
  int a = k - 2;
  for (int i = 1; i <= isqrt(a+1) + 1; i++) {
    if ((a+1) % i == 0) {
      int N = i+1;
      int M = (a+1)/i+1;
      int prod = M*N;
      int sum = M+N+a;
      assert(prod == sum);
#ifdef MYDEBUG
      printf("k=%d a=%d M=%d N=%d\n", k, a, M, N);
#endif
      if (prod < mins[k] || mins[k] == 0)
	mins[k] = prod;
    }
  }
#ifdef MYDEBUG
  printf("\n\n");
#endif
}

void printX(int n) {
  printf("[ ");
  for (int i = 0; i < n; i++)
    printf("%d ", X[i]);
  printf("]\n");
}

void f(int max, int a, int b, int n) {
#ifdef MYDEBUG
  printf("f(max=%d, a=%d, b=%d, n=%d)\n", max, a, b, n);
#endif

  if (n == b+1) {
    int prod, sum;
    prodsum(&prod, &sum, b);
#ifdef MYDEBUG
    printf("k=%d b=%d\n", a+b, b);
    printX(b);
    printf("a=%d prod=%d sum=%d\n", a, prod, sum);
#endif
    int k = a+b;
    if (prod == sum+a &&
	(prod < mins[k] || mins[k] == 0)) {
#ifdef MYDEBUG
      printf("updating min");
#endif
      mins[k] = prod;
    }
#ifdef MYDEBUG
    printf("\n\n\n\n");
#endif
    return;
  }

  int x = max;
  int partprod, partsum;

  while (true) {
    X[n-1] = x;
    prodsum(&partprod, &partsum, n);
    int min_fullprod = partprod * (1 << (b-n));
    int min_fullsum = partsum + 2*(b-n);

#ifdef MYDEBUG
    //printX(n);
    //printf("x=%d, partprod=%d partsum=%d min_fullprod=%d min_fullsum=%d\n", x, partprod, partsum, min_fullprod, min_fullsum);
#endif
    if (min_fullprod <= min_fullsum + a)
      break;
    x--;
  }

#ifdef MYDEBUG
  printf("x%d=%d\n", n, x);
  printX(n);
  printf("\n");
#endif
  while (x >= 2) {
    X[n-1] = x;
    f(x, a, b, n+1);
    x--;
  }
}

void handle_case(int k, int b) {
  for (int i = 0; i < 14; i++) X[i] = 2;
  int a = k - b;
  int max_Xfirst = maxi(a / ((1<<(b-1)) - b), 2);
  f(max_Xfirst, a, b, 1);
}

int main() {
  int max_min = 0;

  for (int k = 2; k <= MAXK; k++) {
    handle_case2(k);

    int maxb = ilog(k, 2) + 1;
    for (int b = 3; b <= maxb; b++) {
      handle_case(k, b);
    }

    printf("min for %d is %d\n", k, mins[k]);
    if (mins[k] >= max_min)
      max_min = mins[k];
  }

  int occupancies[max_min+1];
  memset(occupancies, 0, sizeof(int)*(max_min+1));
  for (int i = 1; i <= MAXK; i++)
    occupancies[mins[i]] = 1;

  int sum = 0;
  for (int i = 0; i < max_min+1; i++)
    if (occupancies[i])
      sum += i;
  printf("%d\n", sum);
}