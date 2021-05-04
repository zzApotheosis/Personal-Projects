#include <stdlib.h>
#include <stdio.h>
#include <systemd/sd-journal.h>

void journald_example() {
    sd_journal_print(LOG_INFO, "SUPER LIGMA LUL");
}
