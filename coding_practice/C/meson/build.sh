#!/bin/sh
export CC="clang"
export CXX="clang++"
export LD="lld"
export AR="llvm-ar"
rm -rf target && meson setup target && meson compile -C target
