// Brute force through pairs of points (x1,y1) and (x2,y2), checking
// whether there are two perpendicular sides (i.e. the difference
// vectors are orthogonal)

#include "util.h"

int dotprod(int x1, int y1, int x2, int y2) {
  return x1*x2 + y1*y2;
}

#define N 50
int main() {
  int count = 0;
  for (int x1 = 0; x1 <= N; x1++) {
    for (int x2 = 0; x2 <= N; x2++) {
      for (int y1 = 0; y1 <= N; y1++) {
	for (int y2 = 0; y2 <= N; y2++) {
	  if (x1 == 0 && y1 == 0) continue;
	  if (x2 == 0 && y2 == 0) continue;
	  if (x1 == x2 && y1 == y2) continue;

	  int dotprod1 = dotprod(x1-0,y1-0,x2-0,y2-0);
	  int dotprod2 = dotprod(x2-x1,y2-y1,-x1,-y1);
	  int dotprod3 = dotprod(x1-x2,y1-y2,-x2,-y2);
	  if (dotprod1 == 0 || dotprod2 == 0 || dotprod3 == 0) {
	    count++;
	  }
	}
      }
    }
  }
  printf("%d\n", count/2);
}