#include <cstdlib>
#include <iostream>
#include <string>

int main(int argc, char * argv[]) {
    std::string msg;
    for (int i = 1; i <= 100; i++) {
        msg = std::string();
        if (i % 3 == 0) {
            msg += "fizz";
        }
        if (i % 5 == 0) {
            msg += "buzz";
        }
        if (msg.size() == 0) {
            msg += std::to_string(i);
        }
        std::cout << msg << std::endl;
    }
    return EXIT_SUCCESS;
}
