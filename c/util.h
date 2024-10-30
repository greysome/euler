// Some personal conventions:
// - Capitalized functions (like Ilog) have multiple outputs which are
//   returned by reference.
// - Non-capitalized functions usually return a single output by value.
//   Sometimes they are specialised versions of the corresponding capitalized
//   function, like ilog.
// - ull's (unsigned long longs) are used as a base type for numbers.
//   However, certain parameters (such as the base in ilog) are plain ints,
//   reflecting the fact that they are often small values.

#include <limits.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>

typedef long long ll;
typedef unsigned long long ull;

// Swap for types that support xor
#define SWAP(a,b) if (a != b) do { a ^= b; b ^= a; a ^= b; } while (false)
// Swap for generic types
#define SWAPT(a,b,T) do { T tmp; tmp = a; a = b; b = tmp; } while (false)

// BASIC OPERATIONS ON NUMBERS -------------------------------------------------- 
ull maxi(ull a, ull b);
ull mini(ull a, ull b);
ull diff(ull a, ull b);
ull fact(ull x);
void Gcd(ull a, ull b, ull *d, ll *x, ll *y);
ull gcd(ull a, ull b);
// Set log=floor(log_b(n)), x=b^log.
void Ilog(ull n, int b, int *log, ull *x);
// Version of intlog that forgets x
int ilog(ull n, int b);
// Set root = floor(n^(1/k)), is_power = [n is perfect kth power]?
void Iroot(ull n, int k, ull *root, bool *is_power);
bool ispower(ull n, int k);
bool issquare(ull n);
// Return floor(n^(1/k))
ull iroot(ull n, int k);
ull isqrt(ull n);
// Compute n^k
ull iexp(ull n, int k);
// Modular exponentiaion
ull modexp(ull a, ull b, ull k);
// Number of digits in n
int len(ull n);
// Concatenate two numbers
ull cat(ull a, ull b);
// Concatenate two numbers, adding k zeros in between
ull cat2(ull a, int k, ull b);
// Sum digits
int sumdigits(ull n);
// Populate arr with the digits of n, returning the number of digits
// NOTE: assumes there is enough space to store the digits
int digits(ull n, int *arr);
// Do a and b have the same digits?
bool samedigits(ull a, ull b);

// OTHER OPERATIONS -------------------------------------------------- 
// Test if n is an s-sided polygon]al number.
// e.g. s=3 tests if n is triangular
bool ispolygonal(ull n, int s);

// RNG -------------------------------------------------- 
void initrng();
int randint(int a, int b);

// PRIMES -------------------------------------------------- 
bool isprime(ull n);
ull getfactor(ull n, bool *is_prime);
ull phi(ull n);
void firstnprimes(ull *arr, int n);
// Populate arr with primes <= n, returning the number of elements
int primestill(ull *arr, int n);

// ARRAYS --------------------------------------------------
// Is x in arr[0]...arr[n-1]?
bool contains(int *arr, int x, int n);
// Modifies the length n array arr in-place to the next permutation in
// lexicographic order. Return false if the permutation in arr is the
// last.
bool nextperm(int *arr, int n);
void rotleft(int *arr, int k, int n);
void reverse(int *arr, int n);
void shuffle(int *arr, int n);
// Are arr1 and arr2 permutations of each other?
bool sameelements(int *arr1, int *arr2, int n);
bool anagrams(char *arr1, char *arr2, int n);

#define FOLD(__op, __arr, __n, __res)					\
  __res = __arr[0];							\
  for (int __i = 1; __i < __n; __i++) {					\
    __res = __op(__res, __arr[__i]);					\
  }

// DIJKSTRA --------------------------------------------------
// Performs Dijkstra algorithm on a set of nodes with distances
// represented by the array of arrays dists. Returns length of
// shortest path from start_node to end_node.
int dijkstra(int **dists, int start_node, int end_node, int n);

// A variant of dijkstra() that computes shortest distances to every
// reachable node.
void Dijkstra(int **dists, int *node_dists, int start_node, int n);

// DIGIT ARRAYS --------------------------------------------------

// `arr` holds the digits in reverse order (each element is one digit).
// Memory for the array is malloced in dafrom() and danew(). As the
// programs are usually quite small, I won't bother freeing them.

// `c` is the current number of digits in the array. I always assume
// that enough memory is allocated in arr to hold c digits. (Thus the
// `max` argument in dafrom() and danew() should be large enough.)

typedef struct {
  int *arr;
  int c;
} da;

da dafrom(ull n, int max);
da danew(int max);
void daprint(da x);
void daadd(da *dest, da x, da y);
// Multiply by digit
void damuld(da *dest, da x, int digit);
// x > y?
bool dagt(da x, da y); 