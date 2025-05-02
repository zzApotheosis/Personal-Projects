#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

char buff[] = "qwer\nasdf\n";

int main(void) {
        int p[2];
        pipe(p);

        if (!fork()) {
                for (int buffsize = strlen(buff), len = 0; buffsize > len;)
                        len += write(p[1], buff + len, buffsize - len);
                return EXIT_SUCCESS;
        }

        close(p[1]);
        FILE * f = fdopen(p[0], "r");
        char buf[100];
        while (fgets(buf, 100, f)) {
                fprintf(stdout, "from child: '%s'\n", buf);
        }
        fprintf(stdout, "\n");
        return EXIT_SUCCESS;
}
