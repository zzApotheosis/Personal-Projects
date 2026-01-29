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
#define SHM_SIZE 256

static void child_process(void)
{
  fprintf(stderr, "This is the child process! (%d)\n", getpid());
  sched_yield(); // Yield the scheduler so the parent process is more likely to perform its setup first

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
  void * const shm = mmap(NULL, SHM_SIZE, PROT_READ, MAP_SHARED, shm_fd, 0);
  if (shm == MAP_FAILED)
    {
      perror("mmap");
      return;
    }
  close(shm_fd);

  /* DEBUG: Read data from shared memory */
  fprintf(stderr, "Reading shared memory from child process: %s\n", (const char * const) shm);

  /* Clean up */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }
  
  exit(EXIT_SUCCESS);
}

static void parent_process(const pid_t cpid)
{
  fprintf(stderr, "This is the parent process! (%d)\n", getpid());

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
  close(shm_fd);
  memset(shm, 0, SHM_SIZE);

  /* DEBUG: Store data in shared memory */
  strncpy(shm, "Is this the Krusty Krab?", SHM_SIZE);
  ((char * const) shm)[SHM_SIZE - 1] = '\0';

  /* TODO: Initialize semaphore within shared memory region */

  /* Join child pid */
  waitpid(cpid, NULL, 0);

  /* Clean up */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }
  if (shm_unlink(SHM_NAME) < 0)
    {
      perror("shm_unlink");
      return;
    }
}

void do_interprocess_semaphore_example(void)
{
  fprintf(stderr, " [*] Starting multi-process semaphore example\n");

  const pid_t cpid = fork();
  if (cpid == 0)
    child_process();
  else
    parent_process(cpid);

  /* Block until child joins parent */



  /* Initialize semaphore within shared memory */

  /* Close semaphore */

  /* Close shared memory */
}

void do_intraprocess_semaphore_example(void)
{

  fprintf(stderr, " [*] Starting multi-thread semaphore example\n");
}
