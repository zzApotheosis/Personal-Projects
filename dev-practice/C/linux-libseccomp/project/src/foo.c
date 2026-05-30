#include <fcntl.h>
#include <seccomp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "foo.h"

#define TEST_FILE "/etc/hostname"

// A rudimentary mimicry of OpenBSD's pledge() for "stdio"
static int
fake_pledge_stdio (void)
{
  scmp_filter_ctx ctx;

  // 1. Initialize the filter.
  // Default action: Kill the process if a syscall isn't explicitly allowed.
  ctx = seccomp_init (SCMP_ACT_KILL);
  if (ctx == NULL)
    {
      perror ("seccomp_init failed");
      return -1;
    }

  // 2. Explicitly allow basic standard I/O and runtime syscalls
  // In a real "stdio" pledge, you'd need a few more, but these cover the
  // basics:
  int allowed_syscalls[] = {
    SCMP_SYS (read),      SCMP_SYS (write), SCMP_SYS (fstat),
    SCMP_SYS (brk),  // Used for memory allocation (malloc)
    SCMP_SYS (mmap), // Also used for memory allocation
    SCMP_SYS (munmap),
    SCMP_SYS (exit_group) // Graceful exit
  };

  int num_syscalls = sizeof (allowed_syscalls) / sizeof (allowed_syscalls[0]);
  for (int i = 0; i < num_syscalls; i++)
    {
      if (seccomp_rule_add (ctx, SCMP_ACT_ALLOW, allowed_syscalls[i], 0) < 0)
        {
          perror ("Failed to add seccomp rule");
          seccomp_release (ctx);
          return -1;
        }
    }

  // 3. Load the filter into the kernel
  if (seccomp_load (ctx) < 0)
    {
      perror ("Failed to load seccomp filter");
      seccomp_release (ctx);
      return -1;
    }

  // 4. Clean up user-space context (the kernel keeps its copy)
  seccomp_release (ctx);
  return 0;
}

int
do_libseccomp (int argc, char *argv[])
{
  for (int i = 1; i < argc; i++)
    fprintf (stderr, "Unused argument: %s\n", argv[i]);

  fprintf (stdout, "1. Before pledge: Standard output works fine.\n");

  // Try opening a file *before* the sandbox is active
  int fd = open (TEST_FILE, O_RDONLY);
  if (fd >= 0)
    {
      fprintf (stdout,
               "2. Before pledge: Successfully opened %s\n", TEST_FILE);
      close (fd);
    }
  else
    {
      perror ("Failed to open file... This is unexpected");
      abort ();
    }

  fprintf (stdout, "3. Activating our mini-pledge sandbox...\n");
  if (fake_pledge_stdio () < 0)
    {
      fprintf (stderr, "Pledge failed!\n");
      return 1;
    }
  fprintf (stdout, "4. Sandbox active! Standard I/O still works.\n");

  // This write will work because 'write' is allowed
  fflush (stdout);

  // 5. Trigger a violation: Try to open a file again
  // Because 'open' or 'openat' is NOT in our allowed list, the kernel will
  // kill us instantly.
  fprintf (stdout,
           "5. Attempting to open a file (this should kill the process)...\n");
  fflush (stdout); // Force printing before crash

  fd = open (TEST_FILE, O_RDONLY);

  // The program will never reach this line
  fprintf (stdout, "6. If you see this, the sandbox failed. FD: %d\n", fd);
  return 0;
}
