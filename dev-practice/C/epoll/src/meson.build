# See https://mesonbuild.com/index.html

project('epoll-example', 'c',
  version: '0.1',
  default_options: ['warning_level=3']
)

executable(meson.project_name(), ['main.c', 'epoll-app.c'], c_args: ['-DPROJECT_NAME="' + meson.project_name() + '"'], install: true, install_rpath: '$ORIGIN:$ORIGIN/../lib64:$ORIGIN/../lib')
