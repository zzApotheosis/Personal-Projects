#!/bin/sh
REALPATH=$(realpath "$0")
CWD=$(dirname "${REALPATH}")
PROJECTDIR="${CWD}"
BUILDDIR="${CWD}/build"
rm -rf "${BUILDDIR}"
cmake -B "${BUILDDIR}" -S "${PROJECTDIR}"
cd "${BUILDDIR}"
make
