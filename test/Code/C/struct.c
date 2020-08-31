
struct Point {
    int x;
    int y;
};

int foo() {
    struct Point p = {4,5};
    // 4
    return p.x;
}

int bar() {
    struct Point p = {4,5};
    p.x = 6;
    // 6
    return p.x;
}

typedef struct Point2s {
    int x;
    int y;
} Point2;

int baz() {
    Point2 p = {4,5};
    p.x = 6;
    // 6
    return p.x;
}
int baz2() {
    struct Point2s p = {4,5};
    p.x = 6;
    // 6
    return p.x;
}

int buffoon() {
    Point2 p = {4,5};
    // 8
    return sizeof(Point2);
}

int orangutan() {
    Point2 p = {4,5};
    // 8
    return sizeof(p);
}

int baboon() {
    Point2 p = {4,5};
    // 4
    return sizeof(p.x);
}

int chimp() {
    Point2 p = {4,5};
    for (int i = 0; i < 3; ++i) {
      p.x += i + 1;
      p.y += i + 2;
    }
    // 4 + 1 + 2 + 3 = 10
    return p.x;
}

int zebra() {
    Point2 p[] = {{4,5}};
    // 4
    return p[0].x;
}

int gazelle() {
    Point2 p[] = {{4,5},{6,7}};
    p[1].x = 8;
    p[0].x = 3;
    // 5 + 7 + 8 + 3 = 23
    return p[0].x + p[0].y + p[1].x + p[1].y;
}

typedef struct Point3s {
    int coords[3];
} Point3;

int cheetah() {
    Point3 p = { { 1, 2, 3 } };
    p.coords[0] = 5;
    // 7
    return p.coords[0] + p.coords[1];
}

int main() {
    return baboon();
}
