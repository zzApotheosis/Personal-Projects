#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>

#include "common-macros.h"
#include "counter.h"

counter_t * counter_new() {
        counter_t * const new_counter = (counter_t *) malloc(1 * sizeof(counter_t));
        if (new_counter == NULL)
                malloc_error();
        memset(new_counter, 0, 1 * sizeof(counter_t));
        pthread_mutex_init(&new_counter->lock, NULL);
        pthread_cond_init(&new_counter->instance_released, NULL);
        return new_counter;
}

void counter_destroy(counter_t * const self) {
        if (self == NULL)
                return;
        free(self);
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

ssize_t counter_get_read_lock(counter_t * const self) {
        pthread_mutex_lock(&self->lock);
        for (;self->locks < 0;) {
                pthread_cond_wait(&self->instance_released, &self->lock);
        }
        self->locks += 1;
        fprintf(stdout, "Current read locks: %02ld\n", self->locks);
        pthread_mutex_unlock(&self->lock); /* Read locks should not have the mutex */
        
        /* TODO: Error checking and return -1 on error */
        return 0;
}

ssize_t counter_get_write_lock(counter_t * const self) {
        /* Somebody's requesting a write lock, so let's wait for all read locks to fuck off */
        pthread_mutex_lock(&self->lock);
        for (;self->locks > 0;) {
                pthread_cond_wait(&self->instance_released, &self->lock);
        }

        self->locks = -1;

        /* TODO: Error checking and return -1 on error */
        return 0;
}

ssize_t counter_release_read_lock(counter_t * const self) {
        if (self == NULL)
                return -1;
        pthread_mutex_lock(&self->lock);
        self->locks -= 1;
        pthread_mutex_unlock(&self->lock);
        pthread_cond_broadcast(&self->instance_released);

        /* TODO: Error checking and return -1 on error */
        return 0;
}

ssize_t counter_release_write_lock(counter_t * const self) {
        if (self == NULL)
                return -1;
        self->locks = 0;
        pthread_mutex_unlock(&self->lock);
        pthread_cond_broadcast(&self->instance_released);

        /* TODO: Error checking and return -1 on error */
        return 0;
}
