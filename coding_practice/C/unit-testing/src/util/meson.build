project_name = 'unit-testing'
project(project_name, 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

subdir('math')
subdir('test')

executable('unit-testing', ['main.c'], link_with: math_lib)
