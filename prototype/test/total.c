#include <stdio.h>

/*
int total(int a[], int s) {
    int i = 0;
    int sum = 0;
    while (i < s) {
        sum += a[i];
        i++;
    }
    return sum;
}
*/

int total(int a[], int s) {
  __asm__("  push %rbx\n"
          "  mov %rdi, %rbx\n"
          "  mov $0x0, %eax\n"
          "  mov $0x0, %ecx\n"
          "loop:\n"
          "  cmp %esi, %ecx\n"
          "  jge out\n"
          "  add (%rbx), %eax\n"
          "  add $0x4, %rbx\n"
          "  inc %ecx\n"
          "  jmp loop\n"
          "out:\n"
          "  pop %rbx\n");
}

int main(int argc, char* argv[]) {
/*
  int i, n, size;
  int a[] = { 4, 8, 15, 16, 23, 42 };

  size = sizeof(a) / sizeof(int);
  n = total(a, size);
*/
  int a[] = { 10 };
  return total(a, sizeof(a) / sizeof(int));
}
