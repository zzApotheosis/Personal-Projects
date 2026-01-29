#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#define warn(f, m)                                      \
  do {                                                  \
    fprintf(f, "%s:%d %s\n", __FILE__, __LINE__, m);    \
  } while (0);
#define die(f, m)                               \
  do {                                          \
    warn(f, m);                                 \
    abort();                                    \
  } while (0);

#ifndef NUM_THREADS
#define NUM_THREADS 64
#endif

#ifndef MAX_VALUE
#define MAX_VALUE 1337
#endif

#ifndef MAX_SLEEP_TIME
#define MAX_SLEEP_TIME 5000
#endif

#ifndef BREAK_NUM
#define BREAK_NUM 8
#endif

static int thread_count = 0;
static pthread_mutex_t thread_count_mutex = PTHREAD_MUTEX_INITIALIZER;
static int data = 0;
static pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

static pthread_cond_t data_incremented = PTHREAD_COND_INITIALIZER;

static unsigned int counters[NUM_THREADS] = {0};

void * thread_main(void * arg)
{
  if (arg) {} /* Completely unnecessary arg check but it makes GCC and Clang shut up about unused arguments */
  pthread_mutex_lock(&thread_count_mutex);
  int thread_id = thread_count++;
  pthread_mutex_unlock(&thread_count_mutex);

  fprintf(stdout, "Initializing thread #%d\n", thread_id);

  /* We are now at the mercy of the kernel scheduler */
  for (;;)
    {
      usleep(rand() % MAX_SLEEP_TIME);
      pthread_mutex_lock(&data_mutex);
      if (data >= MAX_VALUE)
        {
          pthread_mutex_unlock(&data_mutex);
          break;
        }
      data++;
      fprintf(stdout, "Thread #%02d incremented the shared data to %06d; this thread has contributed %06u times\n",
              thread_id, data, ++counters[thread_id]);
      pthread_mutex_unlock(&data_mutex);
      pthread_cond_broadcast(&data_incremented);
    }

  return NULL;
}

int main(void)
{
  srand((unsigned int) time(NULL));
        
  fprintf(stdout, "Starting main thread\n");
  pthread_t threads[NUM_THREADS];
  for (int i = 0; i < NUM_THREADS; i++)
    {
      pthread_create(&threads[i], 0, thread_main, &i);
    }

  /* Wait for threads to finish */
  pthread_mutex_lock(&data_mutex);
  for (;data < MAX_VALUE;)
    {
      pthread_cond_wait(&data_incremented, &data_mutex);
    }
  pthread_mutex_unlock(&data_mutex);
        
  for (int i = 0; i < NUM_THREADS; i++)
    pthread_join(threads[i], NULL);

  /* Show thread statistics */
  fprintf(stderr, "\033[4mThread statistics\033[m [thread num : contribution count]\n");
  for (int i = 0; i < NUM_THREADS; i++)
    {
      fprintf(stderr, "[%02d:%03u]", i, counters[i]);
      if (((i + 1) % BREAK_NUM) == 0)
        fprintf(stderr, "\n");
      else
        fprintf(stderr, " ");
    }
  if ((NUM_THREADS % BREAK_NUM) != 0)
    fprintf(stderr, "\n");
  fprintf(stdout, "Main thread finished\n");
  return EXIT_SUCCESS;
}
