project('message_queues', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)

executable('client', ['client.c'], link_args : '-lrt')
executable('server', ['server.c'], link_args : '-lrt')
