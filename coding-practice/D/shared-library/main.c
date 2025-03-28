#include <stdio.h>
#include <dlfcn.h>  // For dynamic loading

int main() {
    // Open the shared library
    void* handle = dlopen("./libmathlib.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Error: %s\n", dlerror());
        return 1;
    }

    // Clear any existing errors
    dlerror();

    // Get function pointers
    int (*add)(int, int) = dlsym(handle, "add");
    double (*multiply)(double, double) = dlsym(handle, "multiply");

    // Check for errors
    char* error = dlerror();
    if (error != NULL) {
        fprintf(stderr, "Error: %s\n", error);
        dlclose(handle);
        return 1;
    }

    // Use the functions
    int sum = add(5, 3);
    double product = multiply(4.5, 2.0);

    printf("Sum: %d\n", sum);          // Should print 8
    printf("Product: %f\n", product);   // Should print 9.0

    // Clean up
    dlclose(handle);
    return 0;
}
