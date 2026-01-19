#!/bin/sh

source './macros.sh'

printf "Exec name: ${_exec_name}\n" 1>&2
printf "Exec path: ${_exec_path}\n" 1>&2
printf "Exec dir: ${_exec_dir}\n" 1>&2

warn "${LINENO}" "This is an example warning!"
die "${LINENO}" "Ow that hurt!"

printf "This statement should be unreachable.\n" 1>&2
