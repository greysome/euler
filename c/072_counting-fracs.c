#include "util.h"

int main() {
  ull sum = 1;
  for (int i = 1; i <= 1000000; i++)
    sum += phi(i);
  printf("%lld\n", sum-2);
}