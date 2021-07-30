#!/bin/sh
BUILDDIR=target
rm -rf ${BUILDDIR} && meson setup ${BUILDDIR} && meson compile -C ${BUILDDIR}
