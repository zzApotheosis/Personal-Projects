#!/bin/sh
# We define the shebang but do not intend to directly execute this file

die() {
    warn "${1}" "${2}"
    exit 1
}

warn() {
    printf "%s:%d %s\n" "${0}" "${1}" "${2}" 1>&2
}

_exec_name=`basename "${0}"`
_exec_path=`realpath "${0}"`
_exec_dir=`dirname "${_exec_path}"`
