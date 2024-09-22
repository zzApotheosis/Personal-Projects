#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <stdio.h>

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

#endif
