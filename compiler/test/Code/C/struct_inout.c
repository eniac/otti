typedef struct Ps {
    int x;
    int y;
} P;

int foo(P p)
{
    return p.x * p.y;
}

P bar(P p)
{
    P o = { p.x * p.y, p.x };
    return o;
}
