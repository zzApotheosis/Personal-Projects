#!/bin/sh
REAL_PATH=$(realpath "$0")
REAL_DIR=$(dirname "${REAL_PATH}")
SOURCE_DIR="${REAL_DIR}/gpgme"
BUILD_DIR="${REAL_DIR}/builddir"
rm -rf ${BUILD_DIR} && meson setup ${BUILD_DIR} ${SOURCE_DIR} && meson compile -C ${BUILD_DIR}
