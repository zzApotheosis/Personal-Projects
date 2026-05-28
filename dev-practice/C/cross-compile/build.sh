#!/bin/sh
EXEC_PATH=`realpath "${0}"`
EXEC_DIR=`dirname "${EXEC_PATH}"`
CROSS_FILE="${EXEC_DIR}/x86_64-w64-mingw32.txt"
SOURCE_DIR="${EXEC_DIR}/src"
BUILD_DIR="${EXEC_DIR}/.build"
DEST_DIR="${EXEC_DIR}/.destdir"
rm -rf "${DEST_DIR}"
rm -rf "${BUILD_DIR}"
meson setup --cross-file "${CROSS_FILE}" "${BUILD_DIR}" "${SOURCE_DIR}"
meson compile -v -C "${BUILD_DIR}"
meson install -C "${BUILD_DIR}" --destdir "${DEST_DIR}"
