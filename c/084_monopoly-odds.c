#include "util.h"

#define GO 0
#define G2J 30
#define JAIL 10

#define C1 11
#define E3 24
#define H2 39
#define R1 5
#define R2 15
#define R3 25
#define R4 35
#define U1 12
#define U2 28

#define TOTALSQUARES 40

int squarecounts[TOTALSQUARES] = {0};
int chancecards[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
int cccards[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

bool iscc(int square) {
  return square == 2 || square == 17 || square == 33;
}

int handlecc(int square) {
  //static int i = 0;
  //i = (i+1) % 16;
  //int card = cccards[i];
  int card = randint(0,15);
  if (card == 0)
    return GO;
  else if (card == 1)
    return JAIL;
  return square;
}

bool ischance(int square, int *nextr, int *nextu) {
  if (square == 7) {
    *nextr = R2;
    *nextu = U1;
    return true;
  }
  else if (square == 22) {
    *nextr = R3;
    *nextu = U2;
    return true;
  }
  else if (square == 36) {
    *nextr = R1;
    *nextu = U1;
    return true;
  }
  return false;
}

int actualsquare(int tentative);
int handlechance(int square, int nextr, int nextu) {
  //static int i = 0;
  //i = (i+1) % 16;
  //int card = chancecards[i];
  int card = randint(0,15);
  if (card == 0) return GO;
  else if (card == 1) return JAIL;
  else if (card == 2) return C1;
  else if (card == 3) return E3;
  else if (card == 4) return H2;
  else if (card == 5) return R1;
  else if (card == 6 || card == 7) return nextr;
  else if (card == 8) return nextu;
  else if (card == 9) return actualsquare((square-3) % TOTALSQUARES);
  return square;
}

int actualsquare(int tentative) {
  int nextr, nextu;
  if (iscc(tentative))
    return handlecc(tentative);
  else if (ischance(tentative, &nextr, &nextu))
    return handlechance(tentative, nextr, nextu);
  else if (tentative == G2J)
    return JAIL;
  return tentative;
}

int nextsquare(int *doubles, int cur) {
  int roll1 = randint(1,4);
  int roll2 = randint(1,4);
  //if (roll1 == roll2)
  //  (*doubles)++;
  //if (*doubles >= 3) {
  //  *doubles = 0;
  //  return JAIL;
  //}
  return actualsquare((cur+roll1+roll2) % TOTALSQUARES);
}

int main() {
  initrng();
  int cursquare = 0;
  int gameiterations = 1000;
  int doubles = 0;

  int curgames = 0;
  while (true) {
    cursquare = 0;
    shuffle(chancecards, 16);
    shuffle(cccards, 16);

#ifdef MYDEBUG
    printf("chance cards: ");
    for (int i = 0; i < 16; i++)
      printf("%d ", chancecards[i]);
    printf("\n\n");
    printf("cc cards:     ");
    for (int i = 0; i < 16; i++)
      printf("%d ", cccards[i]);
    printf("\n");
#endif

    for (int j = 0; j < gameiterations; j++) {
      cursquare = nextsquare(&doubles, cursquare);
      squarecounts[cursquare]++;
    }

    if (curgames % 1000 == 0) {
      printf("GAME %d\n", curgames);
      int sum = 0;
      for (int i = 0; i < TOTALSQUARES; i++)
	sum += squarecounts[i];
      for (int i = 0; i < TOTALSQUARES; i++)
	printf("square %d has %d occurrences (%.2f%%)\n", i, squarecounts[i], ((float)squarecounts[i])/sum*100);

      printf("[");
      for (int i = 0; i < TOTALSQUARES; i++) {
	if (i < TOTALSQUARES-1)
	  printf("%d,", squarecounts[i]);
	else
	  printf("%d", squarecounts[i]);
      }
      printf("]\n\n");
    }

    curgames++;
  }
}