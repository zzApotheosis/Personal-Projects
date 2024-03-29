#ifndef COUNTER_H
#define COUNTER_H

typedef struct counter_t counter_t;

counter_t * counter_new(void);
void counter_destroy(counter_t * const);

ssize_t counter_set_value(counter_t * const, const size_t);
ssize_t counter_get_value(const counter_t * const, size_t * const);
ssize_t counter_increment(counter_t * const, const size_t);

ssize_t counter_get_read_lock(counter_t * const);
ssize_t counter_get_write_lock(counter_t * const);
ssize_t counter_release_read_lock(counter_t * const);
ssize_t counter_release_write_lock(counter_t * const);


#endif
