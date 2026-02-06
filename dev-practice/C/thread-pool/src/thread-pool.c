#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/queue.h>
#include <pthread.h>

#include "thread-pool.h"

/* Define Thread Pool list */
struct thread_pool_node {
  pthread_t thread;
  STAILQ_ENTRY(thread_pool_node) thread_pool_nodes;
};
STAILQ_HEAD(thread_pool_head, thread_pool_node);

/* Define Job Queue list */
struct job_queue_node {
  void (* job)(void * const);
  void * arg;
  STAILQ_ENTRY(job_queue_node) job_queue_nodes;
};
STAILQ_HEAD(job_queue_head, job_queue_node);

/* Define thread_pool_t data structure */
struct thread_pool {
  struct thread_pool_head threads;
  struct job_queue_head job_queue;
  pthread_mutex_t job_mutex;
  pthread_cond_t job_condition;
  int quit;
}; // thread_pool_t

static void * thread_pool_main(void * arg)
{
  if (!arg)
    abort();

  thread_pool_t * const thread_pool = (thread_pool_t * const) arg;
  struct job_queue_node * job_queue_node = NULL;
  void (* job)(void * const) = NULL;
  void * job_arg = NULL;

  /* Process the job queue */
  for (;;)
    {
      pthread_mutex_lock(&thread_pool->job_mutex);
      for (;!thread_pool->quit && STAILQ_EMPTY(&thread_pool->job_queue);)
        pthread_cond_wait(&thread_pool->job_condition, &thread_pool->job_mutex);

      if (thread_pool->quit && STAILQ_EMPTY(&thread_pool->job_queue))
        {
          pthread_mutex_unlock(&thread_pool->job_mutex);
          break;
        }

      job_queue_node = STAILQ_FIRST(&thread_pool->job_queue);
      STAILQ_REMOVE_HEAD(&thread_pool->job_queue, job_queue_nodes);
      job = job_queue_node->job;
      job_arg = job_queue_node->arg;
      free(job_queue_node);
      pthread_mutex_unlock(&thread_pool->job_mutex);

      /* Invoke the task from the job queue */
      job(job_arg);
    }
  return NULL;
}

thread_pool_t * thread_pool_new(const size_t n_threads)
{
  if (n_threads == 0)
    return NULL;
  thread_pool_t * const instance = (thread_pool_t * const) calloc(1, sizeof(thread_pool_t));
  if (instance == NULL)
    abort();

  if (pthread_mutex_init(&instance->job_mutex, NULL) != 0)
    abort();
  if (pthread_cond_init(&instance->job_condition, NULL) != 0)
    abort();

  STAILQ_INIT(&instance->threads);
  STAILQ_INIT(&instance->job_queue);

  for (unsigned int i = 0; i < n_threads; i++)
    {
      struct thread_pool_node * const node = (struct thread_pool_node * const) calloc(1, sizeof(struct thread_pool_node));
      if (node == NULL)
        abort();
      if (pthread_create(&node->thread, NULL, thread_pool_main, instance) != 0)
        abort();
      STAILQ_INSERT_TAIL(&instance->threads, node, thread_pool_nodes);
    }

  return instance;
}

void thread_pool_destroy(thread_pool_t ** const instance)
{
  if (instance == NULL || (*instance) == NULL)
    abort();
  pthread_mutex_lock(&(*instance)->job_mutex);
  (*instance)->quit = 1;
  pthread_mutex_unlock(&(*instance)->job_mutex);
  pthread_cond_broadcast(&(*instance)->job_condition);

  /* Join all threads */
  for (;!STAILQ_EMPTY(&(*instance)->threads);)
    {
      struct thread_pool_node * thread_pool_node = STAILQ_FIRST(&(*instance)->threads);
      STAILQ_REMOVE_HEAD(&(*instance)->threads, thread_pool_nodes);
      pthread_join(thread_pool_node->thread, NULL);
      free(thread_pool_node);
    }

  /* Clean up queues */
  STAILQ_INIT(&(*instance)->threads);
  STAILQ_INIT(&(*instance)->job_queue);
  
  free(*instance);
  *instance = NULL;
}

void thread_pool_enqueue_job(thread_pool_t * const self, void (* const job)(void * const), void * const arg)
{
  if (self == NULL)
    abort();

  pthread_mutex_lock(&self->job_mutex);
  struct job_queue_node * const job_queue_node = (struct job_queue_node * const) malloc(1 * sizeof(struct job_queue_node * const));
  if (job_queue_node == NULL)
    abort();
  job_queue_node->job = job;
  job_queue_node->arg = arg;
  STAILQ_INSERT_TAIL(&self->job_queue, job_queue_node, job_queue_nodes);
  pthread_mutex_unlock(&self->job_mutex);

  pthread_cond_signal(&self->job_condition);
}
