#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

#define ITER 100000
#define NUM_THREADS 4

int shared = 0;
pthread_mutex_t shared_mutex = PTHREAD_MUTEX_INITIALIZER;

void incr_func(int id) {
    pthread_mutex_lock(&shared_mutex); // TRY REMOVING ME
    shared++;
    pthread_mutex_unlock(&shared_mutex); // TRY REMOVING ME
}

void * func(void * arg) {
    int id = *(int *) arg;
    fprintf(stdout, "Inrementing shared value...\n");
    for (int i = 0; i < ITER; i++) {
        incr_func(id);
    }
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
