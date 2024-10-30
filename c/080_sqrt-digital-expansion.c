// Since we need to compute decimal places of sqrt(N), I implemented
// my own bootleg version of digit arrays, which are just C int arrays
// (with length <= 100) with an extra size tag. To simplify the
// implementation of operations like addition and multiplication, I
// make assumptions about the inputs, which are satisfied when the
// algorithm for sqrt(N) is run.
//
// ALGORITHM FOR SQRT(N):
// It is best to illustrate through an example. Suppose we already
// know the integer part of sqrt(N), e.g. sqrt(2) = 1.(something);
// let's now determine the first decimal place A.
//
// We have the equation
// 2 = (1.A)^2  (or more precisely, A is the largest digit s.t. (1.A)^2 < 2)
// and also
// 1 = (1.0)^2  (think of 1.0 as the current approximation to sqrt(2))
// By difference of squares, we have
// 1 = 2-1 = (1.A+1.0)(1.A-1.0) = (2.A)(0.A),
//
// To be precise, A is the largest digit s.t. (2.A)(0.A) < 1. We can
// then try out all possible digits, and we will find that A = 4,
// since (2.4)(0.4) < 1 whereas (2.5)(0.5) > 1.
//
// To find the next digit B, we similarly form equations
// 2 = (1.4B)^2 and 1.96 = (1.40)^2
// => 0.04 = (2.8B)(0.0B)
// => B = 1
//
// and we repeat the process until 100 decimal places are obtained.

#include "util.h"

void digitarr_print(int *arr, int c) {
  for (int i = 0; i < c; i++)
    printf("%d ", arr[i]);
  printf("\n");
}

// Strip leading 0s from arr and then return the actual number of digits.
int digitarr_fix(int *arr, int c) {
  for (int i = 0; i < c; i++) {
    if (arr[i] != 0) {
      memmove(arr, arr+i, (c-i) * sizeof(int));
      return c-i;
    }
  }
  return 1;
}

int digitarr_sub(int *ret, int *arr1, int *arr2, int c1, int c2) {
  assert(c1 >= c2);
  int carry = 0;
  for (int i = c1-1; i >= 0; i--) {
    int x = c1-1-i < c2 ? arr2[c2-c1+i] : 0;
    int amount = arr1[i] - x - carry;
    if (amount < 0) {
      amount += 10;
      carry = 1;
    } 
    else {
      carry = 0;
    }
    ret[i] = amount;
  }
  return digitarr_fix(ret, c1);
}

int digitarr_add(int *ret, int *arr1, int *arr2, int c1, int c2) {
  assert(c1 >= c2);
  int carry = 0;
  for (int i = c1-1; i >= 0; i--) {
    int x = c1-1-i < c2 ? arr2[c2-c1+i] : 0;
    int amount = arr1[i] + x + carry;
    if (amount >= 10) {
      amount -= 10;
      carry = 1;
    } 
    else {
      carry = 0;
    }
    ret[i+1] = amount;
  }
  ret[0] = carry;
  
  return digitarr_fix(ret, c1+1);
}

int digitarr_muldigit(int *ret, int *arr, int digit, int c) {
  int carry = 0;
  for (int i = c-1; i >= 0; i--) {
    int amount = arr[i] * digit + carry;
    carry = amount / 10;
    if (amount >= 10)
      amount %= 10;
    ret[i+1] = amount;
  }
  ret[0] = carry;

  return digitarr_fix(ret, c+1);
}

bool digitarr_gt(int *arr1, int *arr2, int c1, int c2) {
  if (c1 < c2) return false;
  if (c1 > c2) return true;
  for (int i = 0; i < c1; i++) {
    if (arr1[i] > arr2[i])
      return true;
    else if (arr1[i] < arr2[i])
      return false;
  }
  return false;
}

//#define MYDEBUG 0
#define NDIGITS 100
#define N 1000  // A number large enough that we don't have to worry about running out of space
int sqrt_digit_sums(int d) {
  int approx[N]; int ca = 0;
  int D[N]; int cD = 0;
  int D_[N]; int cD_ = 0;
  int diff_squares[N]; int cd;
  int sum[N]; int cs;
  int prod[N]; int cp;

  // Populate integer part of square root
  int isqrt = iroot(d, 2);
  ca = digits(isqrt, approx);
  cD = digits(d*100, D);
  cD_ = digits(isqrt*isqrt*100, D_);

  for (int i = 0; i < NDIGITS; i++) {
#ifdef MYDEBUG
    digitarr_print(approx, ca);
#endif

    approx[ca++] = 0;

    cd = digitarr_sub(diff_squares, D, D_, cD, cD_);
    cs = digitarr_add(sum, approx, approx, ca, ca);

    for (int digit = 9; digit >= 0; digit--) {
      sum[cs-1] = digit;
      cp = digitarr_muldigit(prod, sum, digit, cs);

#ifdef MYDEBUG
      printf("digit=%d:\n", digit);
      printf("D = ");
      digitarr_print(D, cD);
      printf("D_ = ");
      digitarr_print(D_, cD_);
      printf("D - D_ = ");
      digitarr_print(diff_squares, cd);
      printf("approx = ");
      digitarr_print(approx, ca);
      printf("sum = ");
      digitarr_print(sum, cs);
      printf("prod = ");
      digitarr_print(prod, cp);
#endif

      if (!digitarr_gt(prod, diff_squares, cp, cd)) {
	approx[ca-1] = digit;
	break;
      }
    }
    
    // D *= 100
    D[cD++] = 0;
    D[cD++] = 0;

    // D_ *= (D_ + prod) * 100
    cD_ = digitarr_add(D_, D_, prod, cD_, cp);
#ifdef MYDEBUG
    digitarr_print(D_, cD_);
#endif
    D_[cD_++] = 0;
    D_[cD_++] = 0;

#ifdef MYDEBUG
    printf("\n\n");
#endif
  }

  int digitsum = 0;
  for (int i = 0; i < NDIGITS; i++)
    digitsum += approx[i];
  return digitsum;
}

int main() {
  int sum = 0;
  for (int i = 2; i <= 99; i++) {
    int isqrt = iroot(i, 2);
    if (isqrt*isqrt == i) continue;
    sum += sqrt_digit_sums(i);
  }
  printf("%d\n", sum);
}