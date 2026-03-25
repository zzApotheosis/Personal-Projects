#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <string.h>

// Global variables for demonstration
static volatile int running = 1;
static int signal_count = 0;
static pthread_mutex_t count_mutex = PTHREAD_MUTEX_INITIALIZER;

// ============================================================================
// Worker Thread Function
// ============================================================================
void * worker_thread(void * arg) {
  int thread_id = *(int *) arg;
    
  for (;running;)
    {
      // Simulate work
      printf("[Thread %d] Working...\n", thread_id);
      sleep(2);
    }
    
  printf("[Thread %d] Shutting down gracefully\n", thread_id);
  return NULL;
}

// ============================================================================
// Signal Handler Thread Function
// ============================================================================
void * signal_handler_thread(void * arg) {
  sigset_t * mask = (sigset_t *) arg;
  int sig, err;
    
  printf("[Signal Thread] Ready to handle signals (SIGPIPE, SIGINT, SIGTERM)\n");
    
  for (;running;)
    {
      // Synchronously wait for a signal
      // This blocks until a signal in 'mask' is pending
      err = sigwait(mask, &sig);
      
      if (err != 0)
        {
          // Only happens if mask is invalid or signal was not blocked
          fprintf(stderr, "[Signal Thread] sigwait error: %s\n", strerror(err));
          continue;
        }
        
      printf("\n[Signal Thread] ========== Received Signal %d (%s) ==========\n", 
             sig, strsignal(sig));
        
      // ================================================================
      // SAFE ZONE: We can call ANY function here!
      // Traditional signal handlers cannot do this!
      // ================================================================
        
      switch (sig)
        {
        case SIGPIPE:
          printf("[Signal Thread] Handling SIGPIPE - broken pipe detected\n");
          // Safe to use mutex, printf, malloc, etc.
          pthread_mutex_lock(&count_mutex);
          signal_count++;
          pthread_mutex_unlock(&count_mutex);
          break;
                
        case SIGINT:
          printf("[Signal Thread] SIGINT received - initiating graceful shutdown\n");
          running = 0;
          break;
                
        case SIGTERM:
          printf("[Signal Thread] SIGTERM received - initiating graceful shutdown\n");
          running = 0;
          break;
                
        default:
          printf("[Signal Thread] Unexpected signal: %d\n", sig);
          break;
        }
        
      printf("[Signal Thread] Signal handling complete, returning to wait...\n\n");
    }
    
  printf("[Signal Thread] Shutting down\n");
  return NULL;
}

// ============================================================================
// Main Function
// ============================================================================
int main(int argc, char * argv[]) {
  pthread_t worker_threads[3];
  pthread_t signal_thread;
  sigset_t signal_mask;
  int thread_ids[3] = {1, 2, 3};
  int i;
    
  printf("=== Multi-threaded Signal Handling Demo ===\n");
  printf("PID: %d\n", getpid());
  printf("Try: kill -SIGPIPE %d\n", getpid());
  printf("Try: kill -SIGINT %d  (or press Ctrl+C)\n", getpid());
  printf("Try: kill -SIGTERM %d\n\n", getpid());
    
  // ========================================================================
  // STEP 1: Block signals in the main thread BEFORE creating any threads
  // ========================================================================
  // Child threads inherit the signal mask, so they'll also block these signals
    
  sigemptyset(&signal_mask);
  sigaddset(&signal_mask, SIGPIPE);
  sigaddset(&signal_mask, SIGINT);
  sigaddset(&signal_mask, SIGTERM);
    
  if (pthread_sigmask(SIG_BLOCK, &signal_mask, NULL) != 0)
    {
      perror("pthread_sigmask");
      exit(EXIT_FAILURE);
    }
    
  printf("[Main] Signals blocked in main thread\n");
    
  // ========================================================================
  // STEP 2: Create worker threads
  // They inherit the blocked signal mask from main
  // ========================================================================
  for (i = 0; i < 3; i++)
    {
      if (pthread_create(&worker_threads[i], NULL, worker_thread, &thread_ids[i]) != 0)
        {
          perror("pthread_create worker");
          exit(EXIT_FAILURE);
        }
    }
  printf("[Main] Created %d worker threads\n", 3);
    
  // ========================================================================
  // STEP 3: Create the dedicated signal handling thread
  // Pass the signal mask so it knows what to wait for
  // ========================================================================
  if (pthread_create(&signal_thread, NULL, signal_handler_thread, &signal_mask) != 0)
    {
      perror("pthread_create signal thread");
      exit(EXIT_FAILURE);
    }
  printf("[Main] Created signal handler thread\n\n");
    
  // ========================================================================
  // STEP 4: Main thread can do other work or just wait
  // ========================================================================
    
  // Option A: Main thread does work
  // while (running) { do_something(); }
    
  // Option B: Main thread waits for workers
  printf("[Main] Waiting for shutdown signal...\n\n");
  for (i = 0; i < 3; i++)
    {
      pthread_join(worker_threads[i], NULL);
    }
    
  // Workers have exited, now wait for signal thread
  // But signal thread might be blocked in sigwait() forever
  // We need to wake it up or it's already done
    
  running = 0;  // Signal thread checks this after each signal
    
  // If signal thread is still blocked, we might need to send it a signal
  // or use pthread_cancel, but let's just join and see
  pthread_join(signal_thread, NULL);
    
  printf("\n[Main] All threads terminated. Total signals handled: %d\n", signal_count);
  printf("[Main] Exiting cleanly\n");
    
  return 0;
}
