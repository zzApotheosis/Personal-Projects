#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sched.h>
#include <string.h>
#include <semaphore.h>

#include "foo.h"

#define SHM_NAME "/my-shm"
#define SHM_SIZE 1024
#define ITER 10
#define CHILD_COUNT 10
#define EXPECTED ITER * CHILD_COUNT

typedef struct {
  sem_t sem;
  unsigned int n;
} shm_struct;

static void child_process(void * const shm)
{
  shm_struct * const shm_struct_ = (shm_struct * const) shm;
  
  /* Iterate and increment the value in shared memory using semaphores for synchronization */
  for (unsigned int i = 0; i < ITER; i++)
    {
      /* Decrement the semaphore (or wait for it) */
      if (sem_wait(&shm_struct_->sem) < 0)
        {
          perror("sem_wait");
          abort();
        }

      /* Operate on the shared data */
      shm_struct_->n += 1;

      /* Increment the semaphore */
      if (sem_post(&shm_struct_->sem) < 0)
        {
          perror("sem_post");
          abort();
        }
    }

  /* Unmap shared memory region (not needed anymore) */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }
  
  /* Child process is done! */
  exit(EXIT_SUCCESS);
}

void do_interprocess_semaphore_example(void)
{
  fprintf(stderr, " [*] Starting multi-process semaphore example\n");

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

  /* Initialize semaphore within shared memory region */
  shm_struct * const shm_struct_ = (shm_struct * const) shm;
  if (sem_init(&shm_struct_->sem, !0, 1) < 0)
    {
      perror("sem_init");
      return;
    }

  /* Create CHILD_COUNT children to operate on the shared memory */
  pid_t cpids[CHILD_COUNT] = {0};
  for (unsigned int i = 0; i < CHILD_COUNT; i++)
    {
      const pid_t cpid = fork();
      if (cpid == 0)
        child_process(shm);
      fprintf(stderr, "Created child process [%d]\n", cpid);
      cpids[i] = cpid;
    }

  /* Block until children join parent */
  for (unsigned int i = 0; i < CHILD_COUNT; i++)
    {
      if (waitpid(cpids[i], NULL, 0) < 0)
        {
          perror("waitpid");
          return;
        }
    }

  /* Show counter value */
  fprintf(stderr, "Counter value = %u, expected %u\n", shm_struct_->n, EXPECTED);

  /* Unmap shared memory region (not needed anymore) */
  if (munmap(shm, SHM_SIZE) < 0)
    {
      perror("munmap");
      return;
    }
}

void do_intraprocess_semaphore_example(void)
{
  fprintf(stderr, " [*] Starting multi-thread semaphore example\n");
}
