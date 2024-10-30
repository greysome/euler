// Same as 81; Dijkstra.

#include "util.h"

#define N 80  // Order of the matrix M

int M[] = {
#include "pathsum_matrix.txt"
};

int right_idx(int idx) { return idx % N == N-1 ? -1 : idx+1; }
int left_idx(int idx) { return idx % N == 0 ? -1 : idx-1; }
int above_idx(int idx) { return idx < N ? -1 : idx-N; }
int below_idx(int idx) { return idx >= N*N-N ? -1 : idx+N; }

// M is an array with N*N elements
void build_dist_matrix(int **dists, int *M) {
  for (int i = 0; i < N*N; i++) {
    int right = right_idx(i);
    int left = left_idx(i);
    int above = above_idx(i);
    int below = below_idx(i);
    if (left != -1) dists[i][left] = M[left];
    if (right != -1) dists[i][right] = M[right];
    if (above != -1) dists[i][above] = M[above];
    if (below != -1) dists[i][below] = M[below];
  }

#ifdef MYDEBUG
  for (int i = 0; i < N*N; i++) {
    printf("node %d: ", i);
    for (int j = 0; j < N*N; j++) {
      if (dists[i][j] >= 0)
	printf("%dm from %d, ", dists[i][j], j);
    }
    printf("\n");
  }
#endif
}

int main() {
  int *dists[N*N];
  for (int i = 0; i < N*N; i++) {
    dists[i] = malloc(N*N * sizeof(int));
    memset(dists[i], -1, N*N * sizeof(int));
  }
  build_dist_matrix(dists, M);
  printf("%d\n", dijkstra(dists, N*N) + 4445);
}