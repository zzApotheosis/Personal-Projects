#!/bin/sh
rm -rf target && meson setup target src && meson compile -v -C target
