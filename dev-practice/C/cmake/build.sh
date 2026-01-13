#!/bin/sh
BUILDDIR=.build
PROJECTDIR=.
rm -rf "${BUILDDIR}"
cmake -B "${BUILDDIR}" -S "${PROJECTDIR}"
cmake --build "${BUILDDIR}"
