#include <stdio.h>

void swap(int* x, int* y) {
  int tmp = *x;
  *x = *y;
  *y = tmp;
}

void bubble(int* a, int s) {
  int i, j;

  for (i = 0; i < s; i++) {
    for (j = (i+1); j < s; j++) {
      if (a[j] < a[i])
        swap(&(a[i]), &(a[j]));
    }
  }
}

void invert(int* a, int s) {
  int i;

  for (i = 0; i < s/2; i++)
    swap(&(a[i]), &(a[s-i-1]));
}

int main(int argc, char* argv[]) {
  int array[] = { 23, 16, 4, 15, 42, 8 };
  int size = sizeof(array) / sizeof(int);

  bubble(array, size);
  invert(array, size);

  return 0;
}
