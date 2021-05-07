#include <stdlib.h>
#include <stdio.h>
#include <systemd/sd-journal.h>

int main(int argc, char** argv) {
    sd_journal_print(LOG_INFO, "Hello, world! From journald.c");
}

