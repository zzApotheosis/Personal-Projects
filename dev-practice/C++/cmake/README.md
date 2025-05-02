# CMake Example Project

This is a simple example project with CMake that illustrates a few fundamental points:
- Setting up a decent directory structure
- Specifying build type (release vs. debug)
- Setting up a build directory
- Installing/stripping output executables and libraries
- Setting up unit tests (TODO)
- Creating a binary package with CPack (TODO)
  - Note: Other formats are acceptable too. I.e., Linux distribution maintainers will opt for .deb, .rpm, .tar.zst, etc. for packaging which usually has benefits such as dependency resolution metadata, standardization, cryptographic verification, etc.
  
## Building and running this example

Review the `build.sh` script and execute it on Unix-like systems. Feel free to modify the parameters in the build script to change CMAKE\_BUILD\_TYPE.

> $ ./build.sh  
> $ ./.install/bin/app
