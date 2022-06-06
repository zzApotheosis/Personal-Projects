#include <sys/eventfd.h>
#include <unistd.h>
#include <inttypes.h>           /* Definition of PRIu64 & PRIx64 */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>             /* Definition of uint64_t */
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

int
main(int argc, char *argv[])
{
    int efd;
    uint64_t u;
    ssize_t s;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <num>...\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    efd = eventfd(0, 0);
    if (efd == -1)
        handle_error("eventfd");
    switch (fork()) {
        case 0:
            sleep(1);
            for (int j = 1; j < argc; j++) {
                printf("Child writing %s to efd\n", argv[j]);
                u = strtoull(argv[j], NULL, 0);
                        /* strtoull() allows various bases */
                s = write(efd, &u, sizeof(uint64_t));
                if (s != sizeof(uint64_t))
                    handle_error("write");
            }
            printf("Child completed write loop\n");
            exit(EXIT_SUCCESS);
            break;
        default:
            /*
             * NOTE: The parent process attempts to read from the eventfd immediately, but the
             * child process writes to the eventfd after a delay. This can cause undefined
             * behavior for the parent process. I'm leaving this example code as is, but the
             * proper way to use event file descriptors is to assume that a read() call is
             * 1-to-1 mapped to a single write() call.
             */
            printf("Parent about to read\n");
            s = read(efd, &u, sizeof(uint64_t));
            if (s != sizeof(uint64_t))
                handle_error("read");
            printf("Parent read %"PRIu64" (%#"PRIx64") from efd\n", u, u);
            exit(EXIT_SUCCESS);
            break;
        case -1:
            handle_error("fork");
            break;
    }
}
