#include <sys/eventfd.h>
#include <unistd.h>
#include <inttypes.h>           /* Definition of PRIu64 & PRIx64 */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>             /* Definition of uint64_t */
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

int argcheck(int argc, char * argv[], int val) {
    unsigned long long sum = 0;
    for (int i = 0; i < argc; i++) {
        sum += strtoull(argv[i], NULL, 0);
    }
    if (sum == val) {
        return(0);
    } else {
        return(-1);
    }
}

int
main(int argc, char *argv[])
{
    int efd;
    uint64_t u;
    uint64_t sum = 0;
    ssize_t s = 0;
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
             * behavior for the parent process. I have implemented some negligibly simple error
             * checking in order to accommodate this caveat. Ideally, the parent should goto
             * the p_redo label if it detects that it hasn't fully processed all of the argv
             * elements.
             *
             * To see this caveat in action, compile this program with `make` and run the program
             * with these arguments:
             * ./forked 1 2 4 8 16 32 64 128
             *
             * Run the program a few times and you will likely see the read() will only catch
             * (and process) a few write() calls until finally processing all of them.
             */
p_redo:
            printf("Parent about to read\n");
            s = read(efd, &u, sizeof(uint64_t));
            sum += u;
            if (s != sizeof(uint64_t))
                handle_error("read");
            printf("Parent read %"PRIu64" (%#"PRIx64") from efd\n", u, u);
            if (argcheck(argc, argv, sum) != 0) {
                goto p_redo;
            }
            exit(EXIT_SUCCESS);
            break;
        case -1:
            handle_error("fork");
            break;
    }
}
