int test1() {
    int i = 0;
    for (i = 0; i < 4; ++i) {
        if (i % 3 == 2) {
            break;
        }
    }
    return i;
}

int test2() {
    int i = 0;
    int j = 0;
    for (i = 0; i < 4; ++i) {
        if (i % 3 == 2) {
            break;
        }
        j = i;
    }
    return j;
}

int test3() {
    int i = 0;
    int j[4] = {0,0,0,0};
    for (i = 0; i < 4; ++i) {
        if (i % 3 == 2) {
            break;
        }
        j[i] = i;
    }
    int acc = 0;
    for (i = 0; i < 4; ++i) {
        acc += j[i];
    }
    return acc;
}
