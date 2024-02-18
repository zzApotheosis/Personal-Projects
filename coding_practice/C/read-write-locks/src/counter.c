#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>

#include "common-macros.h"
#include "counter.h"

static counter_t * counter_instance = NULL;
static pthread_mutex_t counter_instance_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t counter_instance_released = PTHREAD_COND_INITIALIZER;
static ssize_t counter_instance_locks = 0;

static counter_t * counter_new() {
        counter_t * const new_counter = (counter_t *) malloc(1 * sizeof(counter_t));
        if (new_counter == NULL)
                malloc_error();
        memset(new_counter, 0, 1 * sizeof(counter_t));
        return new_counter;
}

static counter_t * counter_get_instance() {
        if (counter_instance == NULL)
                counter_instance = counter_new();
        return counter_instance;
}

void counter_destroy() {
        if (counter_instance == NULL)
                return;
        free(counter_instance);
        counter_instance = NULL;
}

ssize_t counter_set_value(counter_t * const self, const size_t new_value) {
        if (self == NULL)
                return -1;
        self->value = new_value;
        return 0;
}

ssize_t counter_get_value(const counter_t * const self, size_t * const value_ptr) {
        if (self == NULL)
                return -1;
        if (value_ptr == NULL)
                return -1;
        *value_ptr = self->value;
        return 0;
}

counter_t * counter_get_read_lock() {
        pthread_mutex_lock(&counter_instance_lock);
        for (;counter_instance_locks < 0;) {
                pthread_cond_wait(&counter_instance_released, &counter_instance_lock);
        }
        counter_instance_locks += 1;
        fprintf(stdout, "Current read locks: %02ld\n", counter_instance_locks);
        pthread_mutex_unlock(&counter_instance_lock); /* Read locks should not have the mutex */
        
        counter_t * instance = counter_get_instance();
        return instance;
}

counter_t * counter_get_write_lock() {
        /* Somebody's requesting a write lock, so let's wait for all read locks to fuck off */
        pthread_mutex_lock(&counter_instance_lock);
        for (;counter_instance_locks > 0;) {
                pthread_cond_wait(&counter_instance_released, &counter_instance_lock);
        }

        counter_instance_locks = -1;

        counter_t * instance = counter_get_instance();
        return instance;
}

ssize_t counter_release_read_lock(const counter_t * const self) {
        if (self == NULL)
                return -1;
        pthread_mutex_lock(&counter_instance_lock);
        counter_instance_locks -= 1;
        pthread_mutex_unlock(&counter_instance_lock);
        pthread_cond_broadcast(&counter_instance_released);
        return 0;
}

ssize_t counter_release_write_lock(counter_t * const self) {
        if (self == NULL)
                return -1;
        counter_instance_locks = 0;
        pthread_mutex_unlock(&counter_instance_lock);
        pthread_cond_broadcast(&counter_instance_released);
        return 0;
}
