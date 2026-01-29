#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sched.h>
#include <string.h>

#include "foo.h"

#define SHM_NAME "/my-shm"
#define SHM_SIZE 1024
#define EXAMPLE_DATA "Is this the Krusty Krab?"

static void child_process(void * const shm)
{
  fprintf(stderr, "This is the child process! (%d)\n", getpid());
  sched_yield(); // Yield the scheduler so the parent process is more likely to perform its setup first

  /* DEBUG: Read data from shared memory */
  fprintf(stderr, "Reading shared memory from child process: %s\n", (const char * const) shm);

  /* Unmap shared memory region (not needed anymore) */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }
  
  exit(EXIT_SUCCESS);
}

static void parent_process(const pid_t cpid, void * const shm)
{
  fprintf(stderr, "This is the parent process! (%d)\n", getpid());

  /* Store data in shared memory */
  strncpy(shm, EXAMPLE_DATA, SHM_SIZE);
  ((char * const) shm)[SHM_SIZE - 1] = '\0';

  /* Unmap shared memory region (not needed anymore) */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }

  /* Join child pid */
  waitpid(cpid, NULL, 0);
}

void do_posix_shm_example(void)
{
  fprintf(stderr, " [*] Starting POSIX shared memory example\n");

  /* Initialize POSIX shared memory */
  const int shm_fd = shm_open(SHM_NAME, O_RDWR | O_CREAT, 0600);
  if (shm_fd < 0)
    {
      perror("shm_open");
      return;
    }
  if (ftruncate(shm_fd, SHM_SIZE) < 0)
    {
      perror("ftruncate");
      return;
    }
  void * const shm = mmap(NULL, SHM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (shm == MAP_FAILED)
    {
      perror("mmap");
      return;
    }
  if (close(shm_fd) < 0)
    {
      perror("close");
      return;
    }
  if (shm_unlink(SHM_NAME) < 0)
    {
      perror("shm_unlink");
      return;
    }
  memset(shm, 0, SHM_SIZE);

  const pid_t cpid = fork();
  if (cpid == 0)
    child_process(shm);
  else
    parent_process(cpid, shm);

  /* Block until child joins parent */
}
