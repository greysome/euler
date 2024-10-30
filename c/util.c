#include "util.h"

// BASIC OPERATIONS ON NUMBERS -------------------------------------------------- 

inline ull maxi(ull a, ull b) { return a > b ? a : b; }
inline ull mini(ull a, ull b) { return a < b ? a : b; }
inline ull diff(ull a, ull b) { return a > b ? a-b : b-a; }

ull fact(ull x) {
  if (x < 10) {
    switch (x) {
    case 0: return 1;
    case 1: return 1;
    case 2: return 2;
    case 3: return 6;
    case 4: return 24;
    case 5: return 120;
    case 6: return 720;
    case 7: return 5040;
    case 8: return 40320;
    case 9: return 362880;
    }
  }
  return x * fact(x-1);
}

void Gcd(ull a, ull b, ull *_d, ll *_x, ll *_y) {
  if (b == 0) {
    if (_d != NULL) *_d = a;
    if (_x != NULL) *_x = 1;
    if (_y != NULL) *_y = 0;
    return;
  }
  ull d; ll x, y;
  Gcd(b, a%b, &d, &x, &y);
  if (_d != NULL) *_d = d;
  if (_x != NULL) *_x = y;
  if (_y != NULL) *_y = x - (a/b)*y;
}

inline ull gcd(ull a, ull b) {
  ull d; Gcd(a, b, &d, NULL, NULL);
  return d;
}

inline void Ilog(ull n, int b, int *_log, ull *_x) {
  assert(n > 0 && b > 0);
  ull x = 1; int log = 0;
  while (x*b <= n) { log++; x *= b; }
  if (_log != NULL) *_log = log;
  if (_x != NULL) *_x = x;
}

inline int ilog(ull n, int b) {
  int log; Ilog(n, b, &log, NULL); return log;
}

inline void Iroot(ull n, int k, ull *root, bool *is_power) {
  assert(k >= 0);
  ull low = 0; ull high = n; ull cur = n/2;
  while (true) {
    ull exp = iexp(cur, k);
    if (exp == n) {
      if (root != NULL) *root = cur;
      if (is_power != NULL) *is_power = true;
      return;
    }

    if (high-low <= 1) {
      if (root != NULL) *root = cur;
      if (is_power != NULL) *is_power = false;
      return;
    }

    if (iexp(cur, k) > n) {
      high = cur; cur = (low+high)/2;
    }
    else if (iexp(cur, k) < n) {
      low = cur; cur = (low+high)/2;
    }
  }
}

inline bool ispower(ull n, int k) {
  bool res;
  Iroot(n, k, NULL, &res);
  return res;
}

inline bool issquare(ull n) {
  return ispower(n, 2);
}

inline ull iroot(ull n, int k) {
  ull root;
  Iroot(n, k, &root, NULL);
  return root;
}

inline ull isqrt(ull n) {
  return iroot(n, 2);
}

inline ull iexp(ull n, int k) {
  assert(k >= 0);
  ull res = 1;
  for (int i = 0; i < k; i++) res *= n;
  return res;
}

ull modexp(ull a, ull b, ull k) {
  if (b == 0) return 1;

  int log2; ull x; Ilog(b, 2, &log2, &x);

  // Populate array of powers
  ull powers[64]; // a, a^2, a^4, a^8, ...
  powers[0] = a % k;
  for (int i = 1; i <= log2; i++)
    powers[i] = ((powers[i-1] % k) * (powers[i-1] % k)) % k;

  // Compute a^b = a^{sum of powers of 2} = product of a^{2^m}
  ull res = 1;
  for (int i = log2; i >= 0; i--) {
    if ((1ULL<<i) <= b) {
      b -= 1ULL<<i;
      res = (res*powers[i]) % k;
    }
  }
  return res;
}

inline int len(ull n) {
  return ilog(n, 10) + 1;
}

inline ull cat(ull a, ull b) {
  if (b == 0) return a;
  ull x; Ilog(b, 10, NULL, &x);
  return a*x*10 + b;
}

inline ull cat2(ull a, int k, ull b) {
  if (b == 0) return a*iexp(10,k);
  ull x; Ilog(b, 10, NULL, &x);
  return a*x*iexp(10,k+1) + b;
}

inline int sumdigits(ull n) {
  if (n == 0) return 0;
  int sum = 0;
  while (n >= 1) {
    int log; ull x;
    Ilog(n, 10, &log, &x);
    sum += n / x;
    n %= x;
  }
  return sum;
}

int digits(ull n, int *arr) {
  if (n == 0) return 0;

  int log; ull x;
  Ilog(n, 10, &log, &x);

  for (int i = 0; i <= log; i++) {
    int digit = n / x;
    arr[i] = digit;
    n -= digit * x;
    x /= 10;
  }
  
  return log+1;
}

bool samedigits(ull a, ull b) {
  int l;
  if ((l = len(a)) != len(b)) return false;
  if (sumdigits(a) != sumdigits(b)) return false;
  int A[l]; digits(a, A);
  int B[l]; digits(b, B);
  return sameelements(A, B, l);
}

// OTHER OPERATIONS -------------------------------------------------- 

inline bool ispolygonal(ull n, int s) {
  assert(s >= 1);
  // The formula for the kth s-sided polygon]al number is
  // 1/2 [(s-2)k^2 - (s-4)k].
  // We can equate this with n, obtaining a quadratic ak^2+bk+c=0 with
  // a = s-2, b = -(s-4), c = -2n. Then we check whether the solution
  // for k, given by the quadratic formula, is a positive integer.
  ll a = s-2; ll b = 4-s; ll c = -2*n;
  ull isqrt; bool is_square;
  Iroot(b*b-4*a*c, 2, &isqrt, &is_square);
  if (!is_square) return false;
  return (-b+isqrt) > 0 && (-b+isqrt) % (2*a) == 0;
}

// RNG --------------------------------------------------

void initrng() {
  srandom(time(NULL));
}

// Returns a random integer in [a,b]
int randint(int a, int b) {
  return random() % (b-a+1) + a;
}


// PRIMES -------------------------------------------------- 

static bool miller_rabin_check_base(ull a, ull d, int s, ull n) {
  a %= n;
  if (a == 0) return true;
  ull x = modexp(a, d, n);
  if (x == 1) return true;
  for (int i = 0; i < s; i++) {
    if (x == n-1) return true;
    unsigned __int128 xx = x*x;
    x = xx % n;
  }
  return false;
}

bool isprime(ull n) {
  // Uses a deterministic Miller-Rabin primality test.

  // Compute d, s such that n-1 = 2^s d, s maximal
  ull d = n-1; int s = 0;
  while (d % 2 == 0) { d /= 2; s++; }

  // According to miller-rabin.appspot.com, this set of bases suffices for numbers < 2^64
  if (!miller_rabin_check_base(2,d,s,n)) return false;
  if (!miller_rabin_check_base(325,d,s,n)) return false;
  if (!miller_rabin_check_base(9375,d,s,n)) return false;
  if (!miller_rabin_check_base(28178,d,s,n)) return false;
  if (!miller_rabin_check_base(450775,d,s,n)) return false;
  if (!miller_rabin_check_base(9780504,d,s,n)) return false;
  if (!miller_rabin_check_base(1795265022,d,s,n)) return false;
  return true;
}

static inline ull pr_iter(ull x, ull n, ull c) {
  unsigned __int128 xx = x*x;
  return (xx+c) % n;
} 

ull getfactor(ull n, bool *is_prime) {
  // Use Polland-Rho to obtain a non-trivial factor of n, if n is non-prime
  // TODO: implement Richard Brent's speedup

  // Special case: n=4 results in an infinite loop
  if (n == 4) {
    if (is_prime != NULL) *is_prime = false;
    return 2;
  }

  if (isprime(n)) {
    if (is_prime != NULL) *is_prime = true;
    return n;
  }

  ull c = 1;
  while (true) {
    ull x = 2;
    ull y = x;
    ull d = 1;

    while (d == 1) {
      x = pr_iter(x,n,c);
      y = pr_iter(pr_iter(y,n,c), n, c);
      d = gcd(diff(x,y), n);
    }

    if (d == n) c++;
    else {
      if (is_prime != NULL) *is_prime = false;
      return d;
    }
  }
}

ull phi(ull n) {
  if (n == 1) return 1;

  bool is_prime;
  ull d = getfactor(n, &is_prime);
  if (is_prime) return n-1;

  ull D = gcd(d, n/d);
  return phi(d) * phi(n/d) * D / phi(D);
}

void firstnprimes(ull *arr, int n) {
  arr[0] = 2;
  ull x = 3; int i = 1;
  while (i < n) {
    if (isprime(x)) arr[i++] = x;
    x += 2;
  }
}

int primestill(ull *arr, int n) {
  int idx = 0;
  for (int i = 2; i <= n; i++)
    if (isprime(i))
      arr[idx++] = i;
  return idx;
}

// ARRAYS --------------------------------------------------

bool contains(int *arr, int x, int n) {
  for (int i = 0; i < n; i++)
    if (arr[i] == x)
      return true;
  return false;
}

bool nextperm(int *arr, int n) {
  // Find largest k such that arr[k] < arr[k+1],
  // and largest l such that arr[k] < arr[l].
  int k = -1, l = -1;
  for (int i = n-1; i >= 1; i--) {
    if (arr[i] > arr[i-1]) {
      k = i-1;
      break;
    }
  }
  if (k == -1) return false;

  for (int i = n-1; i >= 1; i--) {
    if (arr[i] > arr[k]) {
      l = i;
      break;
    }
  }

  SWAP(arr[k], arr[l]);
  // Reverse arr[k+1] ... arr[n]
  for (int i = k+1; i < (k+n)/2+1; i++)
    SWAP(arr[i], arr[n-i+k]);

  return true;
}

inline void rotleft(int *arr, int k, int n) {
  int tmp[n]; memcpy(tmp, arr, n*sizeof(int));
  for (int i = 0; i < n; i++)
    arr[i] = tmp[(i+k)%n];
}

inline void reverse(int *arr, int n) {
  for (int i = 0; i < n/2; i++)
    SWAP(arr[i], arr[n-1-i]);
}

inline void shuffle(int *arr, int n) {
  for (int i = 0; i < n-1; i++) {
    int j = randint(i, n-1);
    SWAP(arr[i], arr[j]);
  }
}

static int intcmp(const void *a, const void *b) {
  return *(int *)a >= *(int *)b;
}

static int charcmp(const void *a, const void *b) {
  return *(char *)a >= *(char *)b;
}

inline bool sameelements(int *arr1, int *arr2, int n) {
  qsort(arr1, n, sizeof(int), intcmp);
  qsort(arr2, n, sizeof(int), intcmp);
  for (int i = 0; i < n; i++)
    if (arr1[i] != arr2[i])
      return false;
  return true;
}

inline bool anagrams(char *arr1, char *arr2, int n) {
  int k;
  if ((k = strnlen(arr1, n)) != strnlen(arr2, n))
    return false;
  qsort(arr1, k, sizeof(char), charcmp);
  qsort(arr2, k, sizeof(char), charcmp);
  for (int i = 0; i < k; i++)
    if (arr1[i] != arr2[i])
      return false;
  return true;
}

// DIJKSTRA --------------------------------------------------

#define INF -1  // A convenient sentinel value

typedef struct {
  bool visited;
  int dist;
} DijkstraNode;

static int nearest_unvisited(DijkstraNode *nodes, int n) {
  int mindist = INT_MAX;
  int minidx = -1;
  for (int i = 0; i < n; i++) {
    DijkstraNode node = nodes[i];
    if (!node.visited && node.dist != INF && node.dist < mindist) {
      mindist = node.dist;
      minidx = i;
    }
  }
  return minidx;
}

int dijkstra(int **dists, int start_node, int end_node, int n) {
  DijkstraNode nodes[n];
  for (int i = 0; i < n; i++) {
    nodes[i].visited = false;
    nodes[i].dist = i == start_node ? 0 : INF;
  }

  while (true) {
    int curidx = nearest_unvisited(nodes, n);
    // All nodes that can be visited, have been visited
    if (curidx == -1)
      return nodes[end_node].dist;
    DijkstraNode curnode = nodes[curidx];

    for (int j = 0; j < n; j++) {
      if (j == curidx) continue;
      // If nodes curidx and j are connected, and if
      // (node curidx's dist) + (dist between nodes) < (node j's dist),
      // update node j's dist.
      int dist = dists[curidx][j];
      if (dist != INF) {
	int newdist = curnode.dist + dist;
	if (nodes[j].dist == INF || newdist < nodes[j].dist)
	  nodes[j].dist = newdist;
      }
    }

    nodes[curidx].visited = true;
  }
}

void Dijkstra(int **dists, int *node_dists, int start_node, int n) {
  DijkstraNode nodes[n];
  for (int i = 0; i < n; i++) {
    nodes[i].visited = false;
    nodes[i].dist = i == start_node ? 0 : INF;
  }

  while (true) {
    int curidx = nearest_unvisited(nodes, n);
    if (curidx == -1)
      break;
    DijkstraNode curnode = nodes[curidx];

    for (int j = 0; j < n; j++) {
      if (j == curidx) continue;
      // If nodes curidx and j are connected, and if
      // (node curidx's dist) + (dist between nodes) < (node j's dist),
      // update node j's dist.
      int dist = dists[curidx][j];
      if (dist != INF) {
	int newdist = curnode.dist + dist;
	if (nodes[j].dist == INF || newdist < nodes[j].dist)
	  nodes[j].dist = newdist;
      }
    }

    nodes[curidx].visited = true;
  }

  if (node_dists != NULL)
    for (int i = 0; i < n; i++)
      node_dists[i] = nodes[i].dist;
}


// DIGIT ARRAYS --------------------------------------------------

da dafrom(ull n, int max) {
  int *arr = malloc(max * sizeof(int));
  int c = digits(n, arr);
  reverse(arr, c);
  da da;
  da.arr = arr;
  da.c = c;
  return da;
}

da danew(int max) {
  return dafrom(0, max);
}

void daprint(da x) {
  for (int i = x.c-1; i >= 0; i--)
    printf("%d", x.arr[i]);
}

void daadd(da *dest, da x, da y) {
  if (y.c > x.c) {
    SWAPT(x.arr, y.arr, int*);
    SWAP(x.c, y.c);
  }
  assert(x.c >= y.c);

  int carry = 0;
  for (int i = 0; i < x.c; i++) {
    int amount;
    if (i < y.c)
      amount = x.arr[i] + y.arr[i] + carry;
    else
      amount = x.arr[i] + carry;

    if (amount >= 10) {
      amount -= 10;
      carry = 1;
    } 
    else
      carry = 0;
    dest->arr[i] = amount;
  }
  dest->arr[x.c] = carry;
  dest->c = carry ? x.c+1 : x.c;
}

void damuld(da *dest, da x, int digit) {
  int carry = 0;
  for (int i = 0; i < x.c; i++) {
    int amount = x.arr[i] * digit + carry;
    carry = amount / 10;
    if (amount >= 10)
      amount %= 10;
    dest->arr[i] = amount;
  }
  dest->arr[x.c] = carry;
  dest->c = carry ? x.c+1 : x.c;
}

bool dagt(da x, da y) {
  if (x.c > y.c) return true;
  if (x.c < y.c) return false;
  for (int i = 0; i < x.c; i++) {
    if (x.arr[i] > y.arr[i])
      return true;
    else if (x.arr[i] < y.arr[i])
      return false;
  }
  return false;
}