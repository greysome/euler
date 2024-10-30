#include "util.h"

#define N 1000000
//#define MYDEBUG

ll divisorsums[N+1];
int visited[N+1] = {0};
int chain[10000]; int chainidx = 0;

void set_nth_divisorsum(int n) {
  int total = 0;
  
  for (ll i = 1; i < n; i++) {
    ll idx1 = (3*i*i + i) / 2;
    ll idx2 = (3*i*i - i) / 2;

    ll x1 = 0;
    if (n-idx1 > 0) {
      int entry = divisorsums[n-idx1];
      if (entry == -1)
	set_nth_divisorsum(n-idx1);
      x1 = divisorsums[n-idx1];
    }
    else if (n-idx1 == 0)
      x1 = n;

    ll x2 = 0;
    if (n-idx2 > 0) {
      int entry = divisorsums[n-idx2];
      if (entry == -1)
	set_nth_divisorsum(n-idx2);
      x2 = divisorsums[n-idx2];
    }
    else if (n-idx2 == 0)
      x2 = n;

    if (idx1 > n && idx2 > n)
      break;

    if (i % 2 == 0) total -= x1 + x2;
    else total += x1 + x2;
  }
  divisorsums[n] = total;
}

ll divisorsum(int n) {
  int entry = divisorsums[n];
  if (entry == -1)
    set_nth_divisorsum(n);
  return divisorsums[n];
}

int first_unvisited_idx() {
  for (int i = 0; i <= N; i++) {
    if (!visited[i])
      return i;
  }
  return -1;
}

inline void clear_chain() {
  chainidx = 0;
  memset(chain, 0, sizeof(int)*10000);
}

inline int find_in_chain(int x) {
  for (int i = 0; i < chainidx; i++)
    if (chain[i] == x)
      return i;
  return -1;
}

void print_chain(int x) {
  int orig = x;
  printf("%d ", x);
  do {
    x = divisorsums[x];
    printf("-> %d ", x);
  } while (x != 0 && x <= N && x != orig);
  printf("\n");
}

int main() {
  divisorsums[1] = 1;
  for (int n = 3; n <= N; n++)
    divisorsums[n] = -1;

  for (int n = 2; n <= N; n++) {
    set_nth_divisorsum(n);
    if (n%1000 == 0) printf("s(%d)=%lld\n", n, divisorsums[n]);
  }

  for (int n = 2; n <= N; n++)
    divisorsums[n] -= n;

  // Find the chains
  int curidx;
  int maxlen = 0;
  int maxentry = 1;

  visited[0] = 1;
  for (int i = 1; i <= N; i++) {
    if (i%1000 == 0) printf("%d\n", i);
    if (visited[i]) continue;

    curidx = i;
    while (true) {
#ifdef MYDEBUG
      printf("curidx = %d\n", curidx);
#endif

      if (0 <= curidx && curidx <= N)
	visited[curidx] = 1;

      if (curidx <= 1 || curidx > N) {
#ifdef MYDEBUG
	printf("not a chain\n\n");
#endif
	clear_chain();
	break;
      }

#ifdef MYDEBUG
      printf("chain = [ ");
      for (int i = 0; i < chainidx; i++)
	printf("%d ", chain[i]);
      printf("]\n");
#endif

      int res = find_in_chain(curidx);
#ifdef MYDEBUG
      printf("res = %d\n", res);
#endif

      if (res >= 0) {
	int len = chainidx - res;
	if (len > maxlen) {
	  printf("found length-%d chain\n", len);
	  print_chain(curidx);
	  maxlen = len;
	  maxentry = chain[res];
	}
	clear_chain();
#ifdef MYDEBUG
	printf("\n");
#endif
	break;
      }
      else {
	chain[chainidx++] = curidx;
#ifdef MYDEBUG
	printf("chain = [ ");
	for (int i = 0; i < chainidx; i++)
	  printf("%d ", chain[i]);
	printf("]\n");
#endif
	ll sum = divisorsums[curidx];
	if (sum > N)
	  curidx = 0;
	else
	  curidx = (int)sum;
#ifdef MYDEBUG
	printf("nextidx = %d\n", curidx);
#endif
      }

#ifdef MYDEBUG
      printf("\n");
#endif
    }
  }

  printf("%d\n", maxlen);
  print_chain(maxentry);
}