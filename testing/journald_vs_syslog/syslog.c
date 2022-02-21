#include <stdlib.h>
#include <stdio.h>
#include <syslog.h>

int main(int argc, char** argv) {
    setlogmask(LOG_UPTO(LOG_NOTICE));
    openlog("syslog", LOG_CONS | LOG_PID | LOG_NDELAY, LOG_LOCAL1);
    syslog(LOG_NOTICE, "Hello, world! From syslog.c");
    syslog(LOG_INFO, "This message should not reach the system logs!");
    closelog();
}

