#include <stdio.h>
#include <stdlib.h>

void bubble(int* a, int s) {
  int i, j;

  for (i = 0; i < s; i++) {
    for (j = (i+1); j < s; j++) {
      if (a[j] < a[i]) {
        int tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
      }
    }
  }
}

int main(int argc, char* argv[]) {
  int* array, i;

  array = (int*) malloc((argc-1) * sizeof(int));
  for (i = 1; i < argc; i++)
    array[i-1] = atoi(argv[i]);

  bubble(array, argc-1);

  for (i = 1; i < argc; i++)
    printf("%d ", array[i-1]);

  printf("\n");
  free(array);

  return 0;
}

