#!/bin/sh
BUILD_DIR="build"
PROJECT_DIR="."
rm -rf "$BUILD_DIR"
cmake -B "$BUILD_DIR" -S "$PROJECT_DIR"
cd "$BUILD_DIR"
make
