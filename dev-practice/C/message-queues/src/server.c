#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <mqueue.h>
#include <string.h>

#include "common.h"

int main(int argc, char * argv[]) {
    mqd_t mq;
    struct mq_attr attr;
    char buffer[MAX_SIZE + 1];
    int must_stop = 0;
    memset(buffer, 0, sizeof(buffer));

    attr.mq_flags = 0;
    attr.mq_maxmsg = 10;
    attr.mq_msgsize = MAX_SIZE;
    attr.mq_curmsgs = 0;

    mq = mq_open(QUEUE_NAME, O_CREAT | O_RDONLY, 0644, &attr);
    CHECK((mqd_t)-1 != mq);

    do {
        ssize_t bytes_read;

        bytes_read = mq_receive(mq, buffer, MAX_SIZE, NULL);
        CHECK(bytes_read >= 0);

        buffer[bytes_read] = '\0'; // Set null terminator
        if (!strncmp(buffer, MSG_STOP, strlen(MSG_STOP))) {
            must_stop = 1;
        } else {
            fprintf(stdout, "Received: %s\n", buffer);
        }
    } while (!must_stop);

    CHECK((mqd_t)-1 != mq_close(mq));
    CHECK((mqd_t)-1 != mq_unlink(QUEUE_NAME));

    return(EXIT_SUCCESS);
}
