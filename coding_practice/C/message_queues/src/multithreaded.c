#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <mqueue.h>
#include <string.h>
#include <pthread.h>
#include <time.h>

#include "common.h"

#define QUEUE_NAME "/CS" // "CS" for client-to-server; NOTE: I would choose a different name if this wasn't a simple example
#define MAX_NUMBER 100
#define MIN_NUMBER 0

#define GUESS_ERROR -1
#define GUESS_CORRECT 0
#define GUESS_TOO_LOW 1
#define GUESS_TOO_HIGH 2

int buffer_looks_like_number(char buffer[], size_t buffer_size) {
    int result = 1; // Assume true
    for (size_t i = 0; i < buffer_size; i++) {
        // Check for newline
        if (buffer[i] == '\n') {
            break;
        }

        // Check for null terminator
        if (buffer[i] == '\0') {
            break;
        }

        // Value MUST be between '0' and '9'
        if (buffer[i] < '0' || buffer[i] > '9') {
            result = 0;
            break;
        }
    }
    return result;
}

void * game_thread_main(void * arg) {
    // Generate a random number
    int number = (rand() % (MAX_NUMBER - MIN_NUMBER + 1)) + MIN_NUMBER;
    
    // Open the message queue
    struct mq_attr attr;
    attr.mq_flags = 0;
    attr.mq_maxmsg = 10;
    attr.mq_msgsize = MAX_SIZE;
    attr.mq_curmsgs = 0;
    mqd_t mq = mq_open(QUEUE_NAME, O_CREAT | O_RDWR, 0600, &attr);
    CHECK(mq != (mqd_t) - 1);
    
    // Loop until the correct number has been guessed
    ssize_t bytes_read = 0;
    int guess = MIN_NUMBER - 1;
    char buffer[MAX_SIZE];
    int index = 0;
    memset(buffer, 0, sizeof(buffer));
    while (guess != number) {
        bytes_read = mq_receive(mq, buffer, MAX_SIZE, NULL);
        CHECK(bytes_read >= 0);

        buffer[bytes_read] = '\0'; // Set null terminator
        
        // Check for "exit"
        if (!strncmp(buffer, MSG_STOP, strlen(MSG_STOP))) {
            break;
        }

        // Check that the received buffer is a number
        if (!buffer_looks_like_number(buffer, sizeof(buffer))) {
            buffer[0] = GUESS_ERROR;
            buffer[1] = '\0';
            CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
            continue;
        }

        // Parse number
        guess = 0;
        index = 0;
        while (buffer[index] >= '0' && buffer[index] <= '9') {
            guess *= 10;
            guess += buffer[index++] - '0';
        }

        // Check that the guess matches the correct answer
        memset(buffer, 0, sizeof(buffer));
        if (guess == number) {
            // Nice!
            buffer[0] = GUESS_CORRECT;
            CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
        } else if (guess < number) {
            buffer[0] = GUESS_TOO_LOW;
            CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
        } else if (guess > number) {
            buffer[0] = GUESS_TOO_HIGH;
            CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
        }
    }

    // Close the message queue
    CHECK(mq_close(mq) != (mqd_t) - 1);
    CHECK(mq_unlink(QUEUE_NAME) != (mqd_t) - 1);

    return NULL;
}

int main(int argc, char * argv[]) {
    srand(time(0));

    pthread_t game_thread;
    pthread_create(&game_thread, 0, game_thread_main, 0);

    // Sleep to allow the game thread to initialize
    usleep(100);
    
    mqd_t mq = mq_open(QUEUE_NAME, O_RDWR);
    CHECK(mq != (mqd_t) - 1);

    // Begin game; Note that this thread has zero knowledge of the correct answer! The correct answer is on the stack of the other thread
    char buffer[MAX_SIZE];
    memset(buffer, 0, sizeof(buffer));
    ssize_t bytes_read = 0;
    fprintf(stdout, "Try to guess the number! The correct answer is between %d and %d. Type \"%s\" to quit early.\n", MIN_NUMBER, MAX_NUMBER, MSG_STOP);
    while (1) {
        fprintf(stdout, "Enter a number: ");
        fgets(buffer, MAX_SIZE, stdin);

        // Check for EOF
        if (feof(stdin)) {
            snprintf(buffer, MAX_SIZE, MSG_STOP);
            CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0));
            break;
        }

        // Send guess
        CHECK(0 <= mq_send(mq, buffer, MAX_SIZE, 0)); // Send the guess to the game thread

        // Check for "exit"
        if (!strncmp(buffer, MSG_STOP, strlen(MSG_STOP))) {
            break;
        }

        // Receive response
        bytes_read = mq_receive(mq, buffer, MAX_SIZE, NULL);
        CHECK(bytes_read >= 0);
        buffer[bytes_read] = '\0'; // Set null terminator
        
        if (buffer[0] == GUESS_CORRECT) {
            fprintf(stdout, "That was the answer!\n");
            break;
        } else if (buffer[0] == GUESS_TOO_LOW) {
            fprintf(stdout, "Too low. Try again.\n");
        } else if (buffer[0] == GUESS_TOO_HIGH) {
            fprintf(stdout, "Too high. Try again.\n");
        } else if (buffer[0] == GUESS_ERROR) {
            fprintf(stdout, "Bad entry! Did you enter a number? Try again.\n");
        }
    }

    CHECK((mqd_t)-1 != mq_close(mq));

    pthread_join(game_thread, NULL);

    return EXIT_SUCCESS;
}
