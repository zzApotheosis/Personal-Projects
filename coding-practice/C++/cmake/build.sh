#!/bin/sh

EXEC_PATH=`realpath "${0}"`
EXEC_DIR=`dirname ${EXEC_PATH}`

BUILD_TYPE="Release" # Change to "Debug" or "MinSizeRel" or another build type for different compilation configurations
CMAKE_GENERATOR="Ninja" # Change to "Unix Makefiles" if Ninja unavailable
PROJECT_DIR="${EXEC_DIR}/project"
BUILD_DIR="${EXEC_DIR}/.build"
INSTALL_DIR="${EXEC_DIR}/.install"

case "${BUILD_TYPE}" in
    "Release" | "MinSizeRel")
	STRIP="--strip"
	;;
    *)
	STRIP=
	;;
esac

make_package() {
    cd "${BUILD_DIR}"
    cpack
    cd "${EXEC_DIR}"
}

# Invoke CMake
cmake \
    -B "${BUILD_DIR}" \
    -S "${PROJECT_DIR}" \
    -G "${CMAKE_GENERATOR}" \
    -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
    -DCMAKE_INSTALL_PREFIX="${INSTALL_DIR}" \
    &&
    
    cmake --build "${BUILD_DIR}" &&
    cmake --install "${BUILD_DIR}" ${STRIP} &&
    make_package

