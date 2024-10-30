// I turn each pair of anagrams into an array of placeholders,
// e.g. CAT and ACT => {-1,-2,-3}, {-2,-1,-3}
// Then I iterate thorugh all the possible substitutions to these
// placeholders. Brute force is fast enough to give the asnwer.

#include "util.h"

int maxsquare = 0;

char words[][20] = {
  #include "0098_words.txt" 
};

bool nextarrangement(int *arr, int min, int max, int n) {
  for (int i = n-1; i >= 0; i--) {
    if (arr[i] < max) {
      arr[i]++;
      return true;
    }
    else 
      arr[i] = min;
  }
  return false;
}

// Find the next arrangement with all digits unique
bool nextgoodarrangement(int *arr, int min, int max, int n) {
  while (true) {
    if (!nextarrangement(arr, min, max, n)) return false;
    for (int i = 0; i < n; i++)
      for (int j = i+1; j < n; j++)
	if (arr[i] == arr[j]) goto noreturn;
    return true;
noreturn: {}
  }
}

void substitute(int *subs, int *placeholders, int n) {
  for (int i = 0; i < n; i++) {
    placeholders[i] = subs[-placeholders[i]-1];
  }
}

int fromdigits(int *arr, int n) {
  int value = 1;
  int num = 0;
  for (int i = n-1; i >= 0; i--) {
    num += arr[i] * value; 
    value *= 10;
  }
  return num;
}

void handle(char *w1, char *w2, int n) {
  // Arrays specifying placeholders for digits
  int places1[n], places2[n];
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      if (w1[j] == w1[i])
	places1[j] = -i-1;
    }
    for (int j = 0; j < n; j++) {
      if (w2[j] == w1[i])
	places2[j] = -i-1;
    }
  }

  // Replace -1 in placesX[n] with subs[0], -2 with subs[1], etc.
  int subs[n];
  for (int i = 0; i < n; i++)
    subs[i] = i;

  int A[n], B[n];
  do {
    memcpy(A, places1, n*sizeof(int));
    memcpy(B, places2, n*sizeof(int));
    substitute(subs, A, n);
    substitute(subs, B, n);
    if (A[0] == 0 || B[0] == 0) continue;
    int a = fromdigits(A, n);
    int b = fromdigits(B, n);
    if (issquare(a) && issquare(b)) {
      int m = maxi(a,b);
      if (m > maxsquare)
	maxsquare = m;
    }
  } while (nextgoodarrangement(subs, 0, 9, n));
}

int main() {
  int nwords = sizeof(words)/20;
  char w1[20+1], w2[20+1];
  for (int i = 0; i < nwords; i++) {
    for (int j = i+1; j < nwords; j++) {
      strcpy(w1, words[i]);
      strcpy(w2, words[j]);
      if (anagrams(w1, w2, 20)) {
	printf("%s %s\n", words[i], words[j]);
	handle(words[i], words[j], strlen(words[i]));
      }
    }
  }
}