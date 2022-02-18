struct P {
    int x, y;
};

struct T {
    struct P p, q;
};

int foo(struct P p) {
    return p.x + p.y;
}

int nested(struct T t) {
    return foo(t.p) - foo(t.q);
}
