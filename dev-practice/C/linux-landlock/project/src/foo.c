#define _GNU_SOURCE
// #define __linux__
#include <assert.h>
#include <fcntl.h>
#include <linux/landlock.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

#include "foo.h"

#define TEST_BREAKOUT_FILE "/etc/shadow"

/* Landlock is invoked via syscalls, as glibc wrappers might not be available
 */
static inline int
landlock_create_ruleset (const struct landlock_ruleset_attr *const attr,
                         const size_t size, const __u32 flags)
{
  return syscall (__NR_landlock_create_ruleset, attr, size, flags);
}

static inline int
landlock_add_rule (const int ruleset_fd,
                   const enum landlock_rule_type rule_type,
                   const void *const rule_attr, const __u32 flags)
{
  return syscall (__NR_landlock_add_rule, ruleset_fd, rule_type, rule_attr,
                  flags);
}

static inline int
landlock_restrict_self (const int ruleset_fd, const __u32 flags)
{
  return syscall (__NR_landlock_restrict_self, ruleset_fd, flags);
}

int
do_landlock (int argc, char *argv[])
{
  if (argc < 3)
    {
      fprintf (stderr, "Usage: %s <sandbox directory> <sandbox test file>\n",
               argv[0]);
      return 1;
    }

  const char *allowed_path = argv[1];

  /* 1. Define what actions we want to restrict.
        For a simple 'unveil' read-only replication, we restrict reading files
     and directories. */
  struct landlock_ruleset_attr ruleset_attr = {
    .handled_access_fs
    = LANDLOCK_ACCESS_FS_READ_FILE | LANDLOCK_ACCESS_FS_READ_DIR,
  };

  int ruleset_fd
      = landlock_create_ruleset (&ruleset_attr, sizeof (ruleset_attr), 0);
  if (ruleset_fd < 0)
    {
      perror ("Failed to create landlock ruleset (Are you on Linux 5.13+?)");
      return 1;
    }

  /* 2. Open the directory we want to allow access to and get its file
   * descriptor */
  int allowed_fd = open (allowed_path, O_PATH | O_CLOEXEC);
  if (allowed_fd < 0)
    {
      perror ("Failed to open allowed path");
      close (ruleset_fd);
      return 1;
    }

  /* 3. Create a rule that allows our handled actions on this specific
   * directory tree */
  struct landlock_path_beneath_attr path_beneath = {
    .allowed_access
    = LANDLOCK_ACCESS_FS_READ_FILE | LANDLOCK_ACCESS_FS_READ_DIR,
    .parent_fd = allowed_fd,
  };

  if (landlock_add_rule (ruleset_fd, LANDLOCK_RULE_PATH_BENEATH, &path_beneath,
                         0)
      < 0)
    {
      perror ("Failed to add rule to ruleset");
      close (allowed_fd);
      close (ruleset_fd);
      return 1;
    }
  close (allowed_fd); // We don't need this FD open anymore

  /* 4. Prevent the process from gaining privileges (required before enforcing
   * Landlock) */
  if (prctl (PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0)
    {
      perror ("Failed to set no_new_privs");
      close (ruleset_fd);
      return 1;
    }

  /* 5. Enforce the sandbox! From this point on, filesystem restrictions apply.
   */
  if (landlock_restrict_self (ruleset_fd, 0) < 0)
    {
      perror ("Failed to enforce ruleset");
      close (ruleset_fd);
      return 1;
    }
  close (ruleset_fd);

  printf ("Sandbox enforced! Only allowed to read within: %s\n", allowed_path);

  /* --- DEMO: Let's test the sandbox --- */
  printf ("\n--- Test 1: Trying to read %s (Should fail if not the "
          "allowed path) ---\n",
          TEST_BREAKOUT_FILE);
  FILE *f1 = fopen (TEST_BREAKOUT_FILE, "r");
  if (f1)
    {
      printf ("Unexpected success... Read %s\n", TEST_BREAKOUT_FILE);
      fclose (f1);
      assert (0);
    }
  else
    {
      perror ("Open failed as expected");
      assert(1);
    }

  printf ("\n--- Test 2: Trying to read a file inside your allowed directory "
          "---\n");
  char test_file_path[512];
  snprintf (test_file_path, sizeof (test_file_path), "%s/subdir/test.txt",
            allowed_path);

  // Quick and dirty check to see if we can open it
  FILE *f2 = fopen (test_file_path, "r");
  if (f2)
    {
      printf ("Success! Successfully accessed %s\n", test_file_path);
      for (;;)
        {
          char buffer[4096] = { 0 };
          size_t bytes_read = 0;
          if ((bytes_read = fread (buffer, 1, sizeof (buffer), f2)) < 0)
            abort ();
          fwrite (buffer, 1, bytes_read, stdout);
          if (feof (f2))
            break;
        }
      fclose (f2);
      assert(1);
    }
  else
    {
      perror ("Failed to access allowed file");
      assert(0);
    }

  return 0;
}
