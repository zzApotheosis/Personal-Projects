#!/bin/sh

_exec_path=$(realpath "$0")
_exec_dir=$(dirname "${_exec_path}")
_src_dir="${_exec_dir}/src"

_template_file="${_src_dir}/template.sh"
_output_file="${_exec_dir}/binary-payload.sh"

cat "${_template_file}" > "${_output_file}"
(echo 'hello world!!'; dd if=/dev/urandom bs=256 count=1 2> /dev/null) >> "${_output_file}"
chmod u+x "${_output_file}"
