module greetlib;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;

extern(C) int add(const int a, const int b) {
    return a + b;
}

extern(C) char* greet(const(char)* name) {
    if (name is null) {
        return null;
    }

    const char* greetingPrefix = "Hello, ";
    const char* greetingSuffix = "!";

    size_t prefixLength = strlen(greetingPrefix);
    size_t nameLength = strlen(name);
    size_t suffixLength = strlen(greetingSuffix);

    size_t totalLength = prefixLength + nameLength + suffixLength + 1; // +1 for null terminator

    char* result = cast(char*) malloc(totalLength);

    if (result is null) {
        return null; // Handle allocation failure
    }

    strcpy(result, greetingPrefix);
    strcat(result, name);
    strcat(result, greetingSuffix);

    return result;
}

extern(C) void free_greet_result(char* ptr) {
    if (ptr !is null) {
        free(ptr);
    }
}
