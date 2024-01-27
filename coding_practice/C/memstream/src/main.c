/*
 * See the man page for open_memstream(3)
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#define BUFFER_SIZE 256

int main(void) {
        char buffer[BUFFER_SIZE];
        char * stream_buffer = NULL;
        size_t size = 0;
        FILE * stream = NULL;

        /* Open a memory stream */
        stream = open_memstream(&stream_buffer, &size);
        if (stream == NULL) {
                fprintf(stderr, "%s:%d %s\n", __FILE__, __LINE__, strerror(errno));
                abort();
        }
        
        /* Write some data to the stream */
        memset(buffer, 0, BUFFER_SIZE);
        snprintf(buffer, BUFFER_SIZE, "Hello");
        fwrite(buffer, strnlen(buffer, BUFFER_SIZE), 1, stream);
        fflush(stream);
        fprintf(stdout, "stream size: %lu\n", size);
        fprintf(stdout, "stream: %s\n", stream_buffer);
        fprintf(stdout, "\n");

        /* Write more data to the stream */
        memset(buffer, 0, BUFFER_SIZE);
        snprintf(buffer, BUFFER_SIZE, ", world!");
        fwrite(buffer, strnlen(buffer, BUFFER_SIZE), 1, stream);
        fflush(stream);
        fprintf(stdout, "stream size: %lu\n", size);
        fprintf(stdout, "stream: %s\n", stream_buffer);

        /* Close the stream and free its memory */
        fclose(stream);
        free(stream_buffer);
        
        return EXIT_SUCCESS;
}
