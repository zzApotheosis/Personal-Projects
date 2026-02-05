#include <sched.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>
#include <assert.h>

#include "foo.h"
#include "thread-pool.h"

#define N_THREADS 64
#define N_ITER 65536

/* Shared data */
static unsigned long int shared_data = 0;
static pthread_mutex_t shared_mutex = PTHREAD_MUTEX_INITIALIZER;

static void example_job(void * arg)
{
  if (arg) {}
  for (int i = 0; i < N_ITER; i++)
    {
      pthread_mutex_lock(&shared_mutex);
      shared_data += 1;
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
  assert(shared_data == N_THREADS * N_ITER);
  //assert(shared_data == N_THREADS * N_ITER + 1); // Try un-commenting this!
}
