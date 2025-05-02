#include <dlfcn.h>
#include <stdio.h>

int main() {
    void* handle = dlopen("./libfoo.so", RTLD_LAZY);
    if (!handle) {
        printf("Error: %s\n", dlerror());
        return 1;
    }

    int (*add)(int, int) = dlsym(handle, "add");
    const char* (*greet)(const char*) = dlsym(handle, "greet");

    printf("Add: %d\n", add(5, 3));  // Output: Add: 8
    printf("Greet: %s\n", greet("Alice"));  // Output: Greet: Hello, Alice

    dlclose(handle);
    return 0;
}
