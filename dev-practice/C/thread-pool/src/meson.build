# See https://mesonbuild.com/index.html

project('thread-pool-example', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)


thread_pool_lib = library('thread-pool', ['thread-pool.c'], version: '0.0.1', soversion: '0', install: true)

lib = library('foo', ['foo.c'], version: '0.0.1', c_args: ['-DEMIT_STATS'], soversion: '0', link_with: thread_pool_lib, install: true)

exe = executable(meson.project_name(), ['main.c'], link_with: lib, c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
