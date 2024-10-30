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
    if (right != -1) dists[i][right] = M[right];
    if (below != -1) dists[i][below] = M[below];
    if (above != -1) dists[i][above] = M[above];
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

  int node_dists[N*N];
  int min = INT_MAX;
  for (int start_node = 0; start_node < N*N; start_node += N) {
    printf("%d\n", start_node);
    Dijkstra(dists, node_dists, start_node, N*N);
    for (int end_node = N-1; end_node < N*N; end_node += N) {
      int actual_dist = node_dists[end_node] + M[start_node];
      if (actual_dist < min)
	min = actual_dist;
    }
  }
  printf("%d\n", min);
}