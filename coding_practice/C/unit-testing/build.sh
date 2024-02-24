#!/bin/sh
SOURCE_DIR='src'
BUILD_DIR='build'
rm -rf ${BUILD_DIR} && meson setup ${BUILD_DIR} ${SOURCE_DIR} && meson compile -C ${BUILD_DIR}
