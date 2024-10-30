// Brute force is surprisingly fast

#include "util.h"
//#define DBG

int sudokus[50][9][9] = {
#include "p096_sudoku_modified.txt"
};

void print_sudoku(int (*S)[9]) {
  for (int row = 0; row < 9; row++) {
    for (int col = 0; col < 9; col++)
      printf("%d ", S[row][col]);
    printf("\n");
  }
}

bool isvalid(int (*S)[9], int num, int row, int col) {
  for (int i = 0; i < 9; i++) {
    if (S[row][i] == num)
      return false;
    if (S[i][col] == num)
      return false;
  }

  int r = row - row%3;
  int c = col - col%3;
  if (S[r][c] == num || S[r][c+1] == num || S[r][c+2] == num ||
      S[r+1][c] == num || S[r+1][c+1] == num || S[r+1][c+2] == num ||
      S[r+2][c] == num || S[r+2][c+1] == num || S[r+2][c+2] == num)
    return false;
  
  return true;
}

int answer = 0;

bool solve_sudoku(int (*S)[9], int curidx) {
#ifdef DBG
  printf("currently on square %d\n", curidx);
  print_sudoku(S);
  printf("\n\n");
#endif
  
  int row = curidx/9;
  int col = curidx%9;
  for (int num = 1; num <= 9; num++) {
    if (isvalid(S, num, row, col)) {
      S[row][col] = num;

#ifdef DBG
      printf("inserting %d works at %d\n", num, curidx);
      printf("\n\n");
#endif

      bool remainingsolved = true;
      for (int i = curidx+1; i < 81; i++) {
	int row = i/9;
	int col = i%9;
	if (S[row][col] == 0)
	  remainingsolved = false;
      }
      if (remainingsolved) {
	answer += S[0][0]*100 + S[0][1]*10 + S[0][2];
	return true;
      }

      int nextidx = curidx+1;
      for (; curidx < 81; nextidx++) {
	int newrow = nextidx/9;
	int newcol = nextidx%9;
	if (S[newrow][newcol] == 0)
	  break;
      }
      if (solve_sudoku(S, nextidx))
	return true;
    }
    else {
#ifdef DBG
      printf("%d doesn't work at %d\n", num, curidx);
#endif
    }
  }
#ifdef DBG
  printf("backtracking\n");
#endif
  S[row][col] = 0;
  return false;
}

int main() {
  for (int i = 0; i < 50; i++) {
    int (*S)[9] = sudokus[i];
    int firstidx = 0;
    for (; firstidx < 81; firstidx++) {
      int row = firstidx/9;
      int col = firstidx%9;
      if (S[row][col] == 0)
	break;
    }
    solve_sudoku(S, firstidx);
    printf("number %d:\n", i);
    print_sudoku(S);
    printf("\n\n");
  }

  printf("%d\n", answer);
  return 0;
}