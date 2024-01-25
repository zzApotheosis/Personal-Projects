#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <stdio.h>
#include <string.h>
#include <errno.h>

#define warn(f, msg)\
        do {\
                fprintf(f, "%s:%d %s\n", __FILE__, __LINE__, msg);\
        } while (0);

#define handle_error(f)\
        do {\
                fprintf(f, "%s:%d %s\n", __FILE__, __LINE__, strerror(errno));\
                abort();\
        } while (0);

#endif
