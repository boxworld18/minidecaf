int func(int x, int y) {
    return x + y;
}
int main() {
    int a = 0;
    int b = 1;
    int c = a * b - a;
    int d = func(1, 2) + c;
    return d - b + a;
}