#include <cstdlib>
#include <iostream>

namespace hi
{
    void test_func(int i)
    {
        std::cout << "cool function received integer: " << i << std::endl;
    }
}

int main(int argc, char **argv) {
    std::cout << "Hello world!" << std::endl;
    hi::test_func(24);
    return(EXIT_SUCCESS);
}
