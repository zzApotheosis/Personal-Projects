#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#define ITER 100000
#define NUM_THREADS 4

/*
 * If you run this sample code, you may find that your processor will report a rather large
 * number when the first thread finishes executing. This is because all NUM_THREADS threads
 * are racing each other to increment the shared data ITER times. Once the first thread finishes
 * its incrementing work, the other threads will have also incremented a large portion of their
 * share of iterations, which adds up to a rather large initial reported value. The important
 * thing to understand is that every thread respects each others' access to the shared data
 * through the mutex. Because of this, the program is expected to end with a data value equal to
 * ITER * NUM_THREADS.
 */

int shared = 0;
pthread_mutex_t shared_mutex = PTHREAD_MUTEX_INITIALIZER;

void incr_func() {
    pthread_mutex_lock(&shared_mutex); // TRY REMOVING ME
    shared++;
    pthread_mutex_unlock(&shared_mutex); // TRY REMOVING ME
}

void print_func() {
    pthread_mutex_lock(&shared_mutex); // TRY REMOVING ME
    fprintf(stdout, "The shared value is %d at the end of thread ID #%lu\n", shared, pthread_self());
    pthread_mutex_unlock(&shared_mutex); // TRY REMOVING ME
}

void * func(void * arg) {
    fprintf(stdout, "Inrementing shared value...\n");
    for (int i = 0; i < ITER; i++) {
        incr_func();
    }
    print_func();
    return(NULL);
}

int main(int argc, char * argv[]) {
    pthread_t threads[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_create(&threads[i], NULL, func, &i);
    }
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }
    fprintf(stdout, "shared = %d\n", shared);
    return(EXIT_SUCCESS);
}
