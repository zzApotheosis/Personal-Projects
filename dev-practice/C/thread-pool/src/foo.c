#include <sched.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>
#include <assert.h>

#include "foo.h"
#include "thread-pool.h"

#define N_THREADS 8
#define N_ITER 8

/* Shared data */
static unsigned long int shared_data = 0;
static pthread_mutex_t shared_mutex = PTHREAD_MUTEX_INITIALIZER;

static void dummy_code(void * arg)
{
  fprintf(stderr, "Thread [%lu] executing dummy code! Arg: [%p]\n", pthread_self(), arg);
}

void do_thread_pool_example(void)
{
  thread_pool_t * thread_pool = thread_pool_new(N_THREADS);

  for (unsigned long int i = 0; i < N_THREADS; i++)
    {
      thread_pool_enqueue_job(thread_pool, dummy_code, (void * const) i);
    }

  thread_pool_destroy(&thread_pool);
  assert(thread_pool == NULL);
}
