#include <sched.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>
#include <assert.h>

#include "foo.h"
#include "thread-pool.h"

#define N_THREADS 32
#define MULTIPLIER 8192

/* Shared data */
static unsigned long int shared_data = 0;
static pthread_mutex_t shared_mutex = PTHREAD_MUTEX_INITIALIZER;

static void example_job(void * arg)
{
  if (arg) {}
#ifdef EMIT_STATS
  unsigned long int contribution_count = 0;
#endif
  for (;;)
    {
      pthread_mutex_lock(&shared_mutex);
      if (shared_data >= N_THREADS * MULTIPLIER)
        {
          pthread_mutex_unlock(&shared_mutex);
          break;
        }
      shared_data += 1;
#ifdef EMIT_STATS // See meson.build to enable statistics output
      contribution_count += 1;
      fprintf(stdout, "{\"thread-id\": %lu, \"data\": %lu, \"contributions\": %lu}\n", pthread_self(), shared_data, contribution_count);
#endif
      pthread_mutex_unlock(&shared_mutex);
    }
}

void do_thread_pool_example(void)
{
  thread_pool_t * thread_pool = thread_pool_new(N_THREADS);

  for (unsigned long int i = 0; i < N_THREADS; i++)
    thread_pool_enqueue_job(thread_pool, example_job, (void * const) i);

  thread_pool_destroy(&thread_pool);
  assert(thread_pool == NULL);
  assert(shared_data == N_THREADS * MULTIPLIER);
}
