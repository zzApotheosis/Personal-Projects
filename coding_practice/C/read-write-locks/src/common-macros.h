#ifndef COMMON_MACROS_H
#define COMMON_MACROS_H

#define warn(f, msg)\
        do {\
                fprintf(f, "%s:%d %s\n", __FILE__, __LINE__, msg);\
                fflush(f);\
        } while (0);

#define die(f, msg)\
        do {\
                warn(f, msg);\
                abort();\
        } while (0);

#define malloc_error() die(stderr, "malloc error")
#define bad_call() die(stderr, "bad function call")

#endif
