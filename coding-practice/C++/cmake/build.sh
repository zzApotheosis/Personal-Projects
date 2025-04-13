#!/bin/sh

EXEC_PATH=`realpath "${0}"`
EXEC_DIR=`dirname ${EXEC_PATH}`

CMAKE_GENERATOR="Ninja" # Change to "Unix Makefiles" if Ninja unavailable
PROJECT_DIR="${EXEC_DIR}/project"
BUILD_DIR="${EXEC_DIR}/.build"
INSTALL_DIR="${EXEC_DIR}/.install"

# Invoke CMake
cmake \
    -B "${BUILD_DIR}" \
    -S "${PROJECT_DIR}" \
    -G "${CMAKE_GENERATOR}" \
    -DCMAKE_INSTALL_PREFIX="${INSTALL_DIR}" \
    &&
    
    cmake --build "${BUILD_DIR}" &&
    cmake --install "${BUILD_DIR}"
