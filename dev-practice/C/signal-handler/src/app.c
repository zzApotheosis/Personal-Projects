#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>

#include "app.h"
#include "signal-handler.h"

#define SIGNAL_LIMIT 3

typedef struct app_t {
  pthread_t worker_thread;

  int num_signals;

  pthread_mutex_t mutex;
  pthread_cond_t cond;
} app_t;

static void signal_handler_callback(const int s, void * const arg)
{
  app_t * const app = (app_t * const) arg;

  fprintf(stderr, "Caught signal %d\n", s);

  app->num_signals += 1;
  pthread_cond_signal(&app->cond);
}

static int signal_handler_condition(void * const arg)
{
  app_t * const app = (app_t * const) arg;
  return app->num_signals < SIGNAL_LIMIT;
}

static void * worker_thread_main(void * arg)
{
  app_t * const app = (app_t * const) arg;

  pthread_mutex_lock(&app->mutex);
  for (;app->num_signals < SIGNAL_LIMIT;)
    {
      pthread_cond_wait(&app->cond, &app->mutex);
      fprintf(stderr, "[worker] Signals caught: %d\n", app->num_signals);
    }
  pthread_mutex_unlock(&app->mutex);
  
  return NULL;
}

void
signal_handler_impl(const int argc, char * argv[])
{
  (void) argc;
  (void) argv;

  app_t app = { 0 };
  pthread_mutex_init(&app.mutex, NULL);
  pthread_cond_init(&app.cond, NULL);

  signal_handler_t * const handler = signal_handler_get();
  signal_handler_set_full_signal_mask(handler);
  signal_handler_set_callback(handler, signal_handler_callback);
  signal_handler_set_callback_arg(handler, &app);
  signal_handler_set_condition(handler, signal_handler_condition);
  signal_handler_set_condition_arg(handler, &app);
  signal_handler_start(handler);

  if (pthread_create(&app.worker_thread, NULL, worker_thread_main, &app) < 0)
    abort();

  fprintf(stderr, "PID: %d\n", getpid());

  if (pthread_join(app.worker_thread, NULL) < 0)
    abort();
  signal_handler_join(handler);

  fprintf(stderr, "All threads finished!\n");
}
