project('unit-test-example', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

subdir('math')
subdir('test')

main_sources = files('main.c')
project_link_targets = [math_lib]

executable('unit-test-example',
  main_sources,
  link_with : project_link_targets,
  include_directories : ['../../common-macros', 'math'],
  install : true)
