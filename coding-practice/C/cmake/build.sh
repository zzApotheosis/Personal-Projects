#!/bin/sh
BUILDDIR=build
PROJECTDIR=project
rm -rf ${BUILDDIR}
cmake -B ${BUILDDIR} -S ${PROJECTDIR}
cd ${BUILDDIR}
make
