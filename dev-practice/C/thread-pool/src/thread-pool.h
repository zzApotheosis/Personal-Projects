#ifndef THREAD_POOL_H
#define THREAD_POOL_H

#include <stddef.h>

typedef struct thread_pool thread_pool_t;

#ifdef __cplusplus
extern "C" {
#endif

  thread_pool_t * thread_pool_new(const size_t);
  void thread_pool_destroy(thread_pool_t ** const);
  void thread_pool_enqueue_job(thread_pool_t * const, void (* const)(void * const), void * const);

#ifdef __cplusplus
}
#endif

#endif
