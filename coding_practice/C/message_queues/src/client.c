#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <mqueue.h>

#include "common.h"

int main(int argc, char * argv[]) {
    mqd_t mq;
    char buffer[MAX_SIZE];
    memset(buffer, 0, sizeof(buffer));

    mq = mq_open(QUEUE_NAME, O_WRONLY);
    CHECK((mqd_t)-1 != mq);

    fprintf(stdout, "Send to server (enter \"exit\" to stop it):\n");

    do {
        fprintf(stdout, "> ");
        fflush(stdout);

        memset(buffer, 0, sizeof(buffer));
        fgets(buffer, MAX_SIZE, stdin);

        CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
    } while (strncmp(buffer, MSG_STOP, strlen(MSG_STOP)));

    CHECK((mqd_t)-1 != mq_close(mq));

    return(EXIT_SUCCESS);
}
