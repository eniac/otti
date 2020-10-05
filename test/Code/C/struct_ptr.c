struct W { int x; };

void add(struct W * w) {
    w->x += 1;
}

int main() {
    struct W w = { 1 };
    add(&w);
    return w.x;
}

struct A { int x[2]; };

void add_array(struct A * w) {
    w->x[0] += 1;
}

int array() {
    struct A w = { { 1, 2 } };
    add_array(&w);
    return w.x[0];
}
