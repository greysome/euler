#include "util.h"

// First five are the outer slots, next five are the inner pentagon
int slots[10];
int remaining[9] = {1,2,3,4,5,6,7,8,9};

ull maxstring = 0;

int main() {
  while (nextperm(remaining, 9)) {
    // Because we are looking at 16-digit strings, 10 must only appear
    // once in the string, thus it is in an outer slot.
    // WLOG we use the first outer slot.
    slots[0] = 10;
    // Populate all slots
    for (int i = 1; i < 10; i++)
      slots[i] = remaining[i-1];
    // Do they form a magic pentagon ring?
    int sum1 = slots[0]+slots[5]+slots[6];
    int sum2 = slots[1]+slots[6]+slots[7];
    int sum3 = slots[2]+slots[7]+slots[8];
    int sum4 = slots[3]+slots[8]+slots[9];
    int sum5 = slots[4]+slots[9]+slots[5];
    
    if (sum1 != sum2 || sum1 != sum3 || sum1 != sum4 || sum1 != sum5)
      continue;

    int min_outer;
    int outers[] = {slots[1], slots[2], slots[3], slots[4]};
    FOLD(mini, outers, 4, min_outer);

    for (int i = 0; i < 5; i++) {
      if (min_outer == slots[i]) {
	rotleft(slots, i, 5);
	rotleft(&slots[5], i, 5);

	for (int i = 0; i < 10; i++) printf("%d ", slots[i]);
	printf("\n");
      }
    }

    ull string;
    int in_order[] = {slots[0], slots[5], slots[6],
		      slots[1], slots[6], slots[7],
		      slots[2], slots[7], slots[8],
		      slots[3], slots[8], slots[9],
		      slots[4], slots[9], slots[5]};
    FOLD(cat, in_order, 15, string);
    printf("%lld\n", string);

    if (string > maxstring)
      maxstring = string;
  }

  printf("%lld\n", maxstring);
}