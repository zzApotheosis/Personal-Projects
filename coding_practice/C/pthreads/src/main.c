#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef NUM_THREADS
#define NUM_THREADS 4
#endif

#ifndef MAX_VALUE
#define MAX_VALUE 1000000
#endif

static int thread_count = 0;
static pthread_mutex_t thread_count_mutex = PTHREAD_MUTEX_INITIALIZER;
static int data = 0;
static pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread_main(void *arg) {
  pthread_mutex_lock(&thread_count_mutex);
  int thread_id = thread_count++;
  pthread_mutex_unlock(&thread_count_mutex);
  fprintf(stdout, "Initializing thread #%d\n", thread_id);

  /* We are now at the mercy of the kernel scheduler */
  for (;;) {
    pthread_mutex_lock(&data_mutex);
    if (data >= MAX_VALUE) {
        pthread_mutex_unlock(&data_mutex);
        break;
    }
    data++;
    fprintf(stdout, "Thread #%d incremented the shared data to %d\n", thread_id,
            data);
    pthread_mutex_unlock(&data_mutex);
  }

  return NULL;
}

int main(int argc, char *argv[]) {
  fprintf(stdout, "Starting main thread\n");
  pthread_t threads[NUM_THREADS];
  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_create(&threads[i], 0, thread_main, 0);
  }
  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }
  fprintf(stdout, "Main thread finished\n");
  return EXIT_SUCCESS;
}
