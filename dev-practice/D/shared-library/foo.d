extern(C) export:  // extern(C) ensures C-compatible linkage, export makes symbols visible

int add(const int a, const int b) {
    return a + b;
}

double multiply(const double a, const double b) {
    return a * b;
}
