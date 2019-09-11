#include <stdlib.h>

int inc(int x) {
    return x++;
}

int fmap(int x, int (*func)(int)) {
    if (x < 0)
        return (*func)(x);
    else
        exit(1);
}

int main(int argc, char* argv[]) {
    fmap(0, inc);
    fmap(-1, inc);

    return 0;
}
