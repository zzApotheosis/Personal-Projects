# See https://mesonbuild.com/index.html

project('signal-handler-example', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

lib = library('signal-handler', ['signal-handler.c'], version: '0.0.1', soversion: '0', install: true)

exe = executable(meson.project_name(), ['main.c', 'app.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
