/*
 * See the man page for open_memstream(3)
 *
 * Disclaimer: I did pretty much no error handling in this application.
 * A real implementation will have error checking everywhere.
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
        fprintf(stdout, "\n");

        /* Use fread() to get data from the memory stream */
        fprintf(stdout, "Using fread() and fseek() to read from the memory stream:\n");
        memset(buffer, 0, BUFFER_SIZE);
        fprintf(stdout, "Reading 5 bytes from the memory stream:\n");
        fread(buffer, 5, 1, stream);
        fwrite(buffer, BUFFER_SIZE, 1, stdout);
        fwrite("\n", 1, 1, stdout);
        fflush(stdout);
        fprintf(stdout, "Reading as many bytes as possbile to fill the buffer (size: %u):\n", BUFFER_SIZE);
        memset(buffer, 0, BUFFER_SIZE);
        //fseek(stream, 0, SEEK_SET);
        fread(buffer, BUFFER_SIZE, 1, stream);
        fwrite(buffer, BUFFER_SIZE, 1, stdout);
        fwrite("\n\n", 2, 1, stdout);
        fflush(stdout);

        /* Use fseek() and fwrite() to modify the memory stream */
        fprintf(stdout, "Using fseek() and fwrite() to modify the memory stream:\n");
        memset(buffer, 0, BUFFER_SIZE);
        fseek(stream, 7, SEEK_SET);
        snprintf(buffer, BUFFER_SIZE, "Steven! :)");
        fwrite(buffer, strnlen(buffer, BUFFER_SIZE), 1, stream);
        fflush(stream);
        memset(buffer, 0, BUFFER_SIZE);
        fseek(stream, 0, SEEK_SET);
        fread(buffer, BUFFER_SIZE, 1, stream);
        fprintf(stdout, "Modified buffer:\n");
        fwrite(buffer, BUFFER_SIZE, 1, stdout);
        fprintf(stdout, "\n\n");
        fflush(stdout);
        fprintf(stdout, "stream size: %lu\n", size);

        /* Close the stream and free its memory */
        fclose(stream);
        free(stream_buffer);
        
        return EXIT_SUCCESS;
}
