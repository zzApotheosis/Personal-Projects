#!/bin/sh

SOURCE_DIR="src"
BUILD_DIR="build"

CHECK() {
    CODE=$1
    MESSAGE=$2
    if [ ${CODE} -ne 0 ]; then
        echo "Error: ${MESSAGE}"
        exit ${CODE}
    fi
}

rm -rf "${BUILD_DIR}"
CHECK "$?" "Unable to remove build directory"

cmake -B "${BUILD_DIR}" -S "${SOURCE_DIR}"
CHECK "$?" "Unable to create build directory"

cd ${BUILD_DIR}
# CMake generally defaults to Unix makefiles
make
CHECK "$?" "Unable to compile project"
