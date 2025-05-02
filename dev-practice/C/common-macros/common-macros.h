#ifndef COMMON_MACROS_H
#define COMMON_MACROS_H

#include <stdlib.h>
#include <stdio.h>

#define fwarn(f, msg)\
        do {\
                fprintf(f, "%s:%d %s\n", __FILE__, __LINE__, msg);\
        } while (0);

#define fdie(f, msg)\
        do {\
                fwarn(f, msg);\
                abort();\
        } while (0);

#define warn(msg)\
        do {\
                fwarn(stderr, msg);\
        } while (0);

#define die(msg)\
        do {\
                fdie(stderr, msg);\
        } while (0);

#define MALLOC_ERROR_MSG "malloc error"
#define BAD_CALL_MSG "bad function call"

#define malloc_error() die(MALLOC_ERROR_MSG)
#define bad_call() die(BAD_CALL_MSG)

#endif
