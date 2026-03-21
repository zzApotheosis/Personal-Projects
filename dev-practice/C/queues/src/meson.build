project('linked-list', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

add_project_arguments('-DNUM_NODES=16', language: ['c'])

executable('slist-example', ['main.c', 'slist.c'], c_args: ['-DSLIST_EXAMPLE'])

executable('stailq-example', ['main.c', 'stailq.c'], c_args: ['-DSTAILQ_EXAMPLE'])
