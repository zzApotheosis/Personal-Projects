extern(C) export:  // extern(C) ensures C-compatible linkage, export makes symbols visible

int add(int a, int b) {
    return a + b;
}

double multiply(double a, double b) {
    return a * b;
}
