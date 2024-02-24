# Unit Testing
This is a small example of unit testing in the C language. This project maintains a very simple "math" library which implements functions that perform
basic math tasks that can be natively performed in most languages.

The basic principle of unit testing is to break a complex problem and/or system into smaller components and test each one of those components.
If every component behaves as expected, then it can be assumed that the larger system as a whole will behave as expected as well.

When projects scale to larger sizes, unit testing may become more difficult because complex code may rely on complex objects and/or complex execution states.
Unit testing software can be achieved by creating "code stubs" and/or "mock objects" to replicate what a software unit might expect from its execution environment.
Code stubs and mock objects can be created to provide simplified implementations of a real software component to support the goal of testing a specific software unit.

Run `./build.sh` to build the example project. Run `./test.sh` to run the example tests to ensure the software functions as expected.

# Resources
- https://mesonbuild.com/Unit-tests.html
- https://cmake.org/cmake/help/book/mastering-cmake/chapter/Testing%20With%20CMake%20and%20CTest.html
- https://en.wikipedia.org/wiki/Unit\_testing
- https://en.wikipedia.org/wiki/Method\_stub
- https://en.wikipedia.org/wiki/Mock\_object
- https://en.wikipedia.org/wiki/Test\_harness
