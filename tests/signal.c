#include <stdio.h>
#include <signal.h>
#include <unistd.h>

int g = 1;

void handler(int x) {
    g = 0;
}

int main(int argc, char* argv[]) {
    unsigned long long c = 0;

    signal(SIGINT, handler);
    signal(SIGALRM, handler);
    alarm(3);

    while (g) {
      ++c;
    }

    printf("%llu\n", c);
    return 0;
}
