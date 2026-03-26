#ifndef SIGNAL_HANDLER_H
#define SIGNAL_HANDLER_H

#ifdef __cplusplus
#include <csignal>
#else
#include <signal.h>
#endif

typedef struct signal_handler_t signal_handler_t;

#ifdef __cplusplus
extern "C"
{
#endif

  signal_handler_t * signal_handler_get(void);
  void signal_handler_free(signal_handler_t ** const);
  void signal_handler_set_signal_mask(signal_handler_t * const, sigset_t * const);
  void signal_handler_set_full_signal_mask(signal_handler_t * const);
  void signal_handler_set_callback(signal_handler_t * const, void (* const)(const int, void * const));
  void signal_handler_set_callback_arg(signal_handler_t * const, void * const);
  void signal_handler_set_condition(signal_handler_t * const, int (* const)(void * const));
  void signal_handler_set_condition_arg(signal_handler_t * const, void * const);
  void signal_handler_start(signal_handler_t * const);
  void signal_handler_join(signal_handler_t * const);


#ifdef __cplusplus
}
#endif

#endif
