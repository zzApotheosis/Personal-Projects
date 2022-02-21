#!/bin/sh
rm -rf target && meson setup target && meson compile -C target
