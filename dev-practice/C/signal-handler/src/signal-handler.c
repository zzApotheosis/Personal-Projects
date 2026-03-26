#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "signal-handler.h"

struct signal_handler_t {
  sigset_t mask;

  void (* callback)(const int, void * const);
  void * callback_arg;

  int (* condition)(void * const);
  void * condition_arg;

  pthread_t handler_thread;
};

static void * signal_handler_thread_main(void * arg)
{
  signal_handler_t * const instance = (signal_handler_t * const) arg;
  int sig = 0;
  
  if (instance == NULL)
    abort();

  if (instance->callback == NULL || instance->condition == NULL)
    abort();

  for (;instance->condition(instance->condition_arg);)
    {
      if (sigwait(&instance->mask, &sig) < 0)
        {
          perror("sigwait");
          continue;
        }

      instance->callback(sig, instance->callback_arg);
    }
  
  return NULL;
}

signal_handler_t *
signal_handler_get(void)
{
  static signal_handler_t * instance = NULL;
  if (instance == NULL)
    instance = (signal_handler_t *) calloc(1, sizeof(signal_handler_t));
  if (instance == NULL)
    abort();
  return instance;
}

void signal_handler_free(signal_handler_t ** const instance)
{
  if (instance == NULL || *instance == NULL)
    return;
  free(*instance);
  *instance = NULL;
}

void
signal_handler_set_signal_mask(signal_handler_t * const self, sigset_t * const mask)
{
  self->mask = *mask;
  if (pthread_sigmask(SIG_BLOCK, &self->mask, NULL) != 0)
    perror("pthread_sigmask");
}

void
signal_handler_set_full_signal_mask(signal_handler_t * const self)
{
  sigset_t mask = { 0 };
  sigfillset(&mask);
  sigdelset(&mask, SIGKILL);
  sigdelset(&mask, SIGSTOP);
  signal_handler_set_signal_mask(self, &mask);
}

void
signal_handler_set_callback(signal_handler_t * const self, void (* const callback)(const int, void * const))
{
  self->callback = callback;
}

void signal_handler_set_callback_arg(signal_handler_t * const self, void * const callback_arg)
{
  self->callback_arg = callback_arg;
}

void
signal_handler_set_condition(signal_handler_t * const self, int (* const condition)(void * const))
{
  self->condition = condition;
}

void
signal_handler_set_condition_arg(signal_handler_t * const self, void * const condition_arg)
{
  self->condition_arg = condition_arg;
}

void
signal_handler_start(signal_handler_t * const self)
{
  if (pthread_create(&self->handler_thread, NULL, signal_handler_thread_main, self) < 0)
    perror("pthread_create");
}

void
signal_handler_join(signal_handler_t * const self)
{
  if (pthread_join(self->handler_thread, NULL) < 0)
    perror("pthread_join");
}

