#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <time.h>

#include "common-macros.h"
#include "counter.h"

#ifndef NUM_READ_THREADS
#define NUM_READ_THREADS 16
#endif

#ifndef MAX_VALUE
#define MAX_VALUE 5
#endif

#ifndef READER_SLEEP_TIME
#define READER_SLEEP_TIME 500000
#endif

#ifndef WRITER_SLEEP_TIME
#define WRITER_SLEEP_TIME READER_SLEEP_TIME
#endif

static int thread_count = 0;
static pthread_mutex_t thread_count_mutex = PTHREAD_MUTEX_INITIALIZER;

void * thread_reader_main(void * arg) {
        if (arg) {} /* Completely unnecessary arg check but it makes GCC and Clang shut up about unused arguments */
        pthread_mutex_lock(&thread_count_mutex);
        int thread_id = thread_count++;
        pthread_mutex_unlock(&thread_count_mutex);

        fprintf(stdout, "Initializing thread #%d\n", thread_id);

        size_t value = 0;


        /* We are now at the mercy of the kernel scheduler */
        for (;value < MAX_VALUE;) {
                usleep(rand() % READER_SLEEP_TIME);
                const counter_t * const counter = counter_get_read_lock();
                fprintf(stdout, "Thread #%02d reading counter value: ", thread_id);
                counter_get_value(counter, &value);
                fprintf(stdout, "%ld\n", value);
                usleep((rand() % READER_SLEEP_TIME) * 2 / NUM_READ_THREADS);
                counter_release_read_lock(counter);
        }

        return NULL;
}

void * thread_writer_main(void * arg) {
        if (arg) {} /* Completely unnecessary arg check but it makes GCC and Clang shut up about unused arguments */
        fprintf(stdout, "Initializing writer thread\n");
        size_t value = 0;

        /* We are now at the mercy of the kernel scheduler */
        for (;value < MAX_VALUE;) {
                usleep(rand() % WRITER_SLEEP_TIME);
                counter_t * const counter = counter_get_write_lock();
                value++;
                fprintf(stdout, "Iterating writer thread, new value = %lu\n", value);
                counter_set_value(counter, value);
                counter_release_write_lock(counter);
        }

        return NULL;
}

int main(void) {
        srand((unsigned int) time(NULL));
        setbuf(stdout, NULL);
        setbuf(stderr, NULL);
        
        fprintf(stdout, "Starting main thread\n");

        /* Initialize the reader threads */
        pthread_t threads[NUM_READ_THREADS];
        for (int i = 0; i < NUM_READ_THREADS; i++) {
                pthread_create(&threads[i], 0, thread_reader_main, &i);
        }
        pthread_t writer_thread;
        pthread_create(&writer_thread, 0, thread_writer_main, NULL);

        /* Wait for threads to finish */
        for (int i = 0; i < NUM_READ_THREADS; i++) {
                fprintf(stdout, "Joining thread #%d\n", i);
                pthread_join(threads[i], NULL);
        }
        fprintf(stdout, "Joining writer thread\n");
        pthread_join(writer_thread, NULL);

        counter_destroy();
        fprintf(stdout, "Main thread finished\n");
        return EXIT_SUCCESS;
}
