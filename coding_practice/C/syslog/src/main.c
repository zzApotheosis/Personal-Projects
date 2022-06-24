#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <syslog.h>
#include <string.h>
#include <libgen.h>
#include <limits.h>

int main(int argc, char * argv[]) {
    // Define function variables
    size_t l = strlen(argv[0]) + 1;
    unsigned char * exec_path = realpath(argv[0], NULL);
    // According to the man page for basename(), the function has the possibility of
    // modifying the contents of its argument, and it recommends passing in a copy
    // of the string. I don't do that here because I've never seen it modify the argument
    // string. Just be wary in the event it does.
    unsigned char * ident = basename(exec_path);

    // Perform syslog functions
    openlog(ident, LOG_PID | LOG_NDELAY, LOG_USER);
    syslog(LOG_EMERG, "This is an emergency message!");
    syslog(LOG_ALERT, "This is an alert message!");
    syslog(LOG_CRIT, "This is a critical message!");
    syslog(LOG_ERR, "This is an error message!");
    syslog(LOG_WARNING, "This is a warning message!");
    syslog(LOG_NOTICE, "This is a notice message!");
    syslog(LOG_INFO, "This is an informational message!");
    syslog(LOG_DEBUG, "This is a debug message!");
    closelog();

    // Clean up
    free(exec_path);
    return(EXIT_SUCCESS);
}
