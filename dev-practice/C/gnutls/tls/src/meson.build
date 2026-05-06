# See https://mesonbuild.com/index.html

project('gnutls-example', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

gnutls_d = dependency('gnutls')

lib = library('foo', ['foo.c'], version: '0.0.1', soversion: '0', install: true, dependencies: [gnutls_d])

exe_server = executable(meson.project_name() + '-server', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DROLE=0'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')

exe_client = executable(meson.project_name() + '-client', ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"', '-DROLE=1'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
