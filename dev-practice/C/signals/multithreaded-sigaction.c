#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define N_THREADS 3

// ============================================================================
// Configuration
// ============================================================================

static volatile int running = 1;
static pthread_mutex_t log_mutex = PTHREAD_MUTEX_INITIALIZER;

// Array of signals that require immediate process termination
static const int fatal_signals[] = {
  SIGABRT, // Abort
  SIGBUS,  // Bus error
  SIGFPE,  // Floating point exception
  SIGILL,  // Illegal instruction
  SIGQUIT, // Quit
  SIGSEGV, // Segmentation fault
  SIGSYS,  // Bad system call
  SIGTRAP, // Trace/breakpoint trap
  SIGXCPU, // CPU time limit exceeded
  SIGXFSZ, // File size limit exceeded
};

static const char * fatal_signals_str[] = {
  "ABRT", "BUS", "FPE", "ILL", "QUIT", "SEGV", "SYS", "TRAP", "XCPU", "XFSZ",
};

#define FATAL_SIGNALS_COUNT (sizeof(fatal_signals) / sizeof(fatal_signals[0]))

// Array of signals that indicate graceful shutdown
static const int shutdown_signals[] = {
  SIGINT,  // Ctrl+C
  SIGTERM, // Default kill signal
  SIGHUP,  // Hangup (often used for config reload, but also termination)
};

static const char * shutdown_signals_str[] = {
  "INT",
  "TERM",
  "HUP",
};

#define SHUTDOWN_SIGNALS_COUNT (sizeof(shutdown_signals) / sizeof(shutdown_signals[0]))

// ============================================================================
// Helper Functions
// ============================================================================

static int
is_in_array(int sig, const int * arr, size_t count)
{
  for (size_t i = 0; i < count; i++)
    if (arr[i] == sig)
      return 1;
  return 0;
}

static const char *
get_signal_action(int sig)
{
  if (is_in_array(sig, fatal_signals, FATAL_SIGNALS_COUNT))
    return "FATAL - Process will terminate";

  if (is_in_array(sig, shutdown_signals, SHUTDOWN_SIGNALS_COUNT))
    return "SHUTDOWN - Initiating graceful shutdown";

  return "INFORMATIONAL - Logged and ignored";
}

static void
print_signal_info(int sig)
{
  const char * name = strsignal(sig);
  const char * action = get_signal_action(sig);

  fprintf(stderr, "\n");
  fprintf(stderr, "╔══════════════════════════════════════════════════════════════╗\n");
  fprintf(stderr, "║  SIGNAL CAUGHT                                               ║\n");
  fprintf(stderr, "╠══════════════════════════════════════════════════════════════╣\n");
  fprintf(stderr, "║  Number:      %03d                                            ║\n", sig);
  fprintf(stderr, "║  Name:        %-46s ║\n", name);
  fprintf(stderr, "║  Action:      %-46s ║\n", action);
  fprintf(stderr, "╚══════════════════════════════════════════════════════════════╝\n");
  fprintf(stderr, "\n");
}

// ============================================================================
// Worker Thread Function
// ============================================================================

static void *
worker_thread(void * arg)
{
  int thread_id = *(int *) arg;
  int iteration = 0;

  usleep(1000);

  for (; running;)
    {
      iteration++;
      fprintf(stderr, "[Worker %d] Iteration %d - working...\n", thread_id, iteration);
      sleep(3);
    }

  fprintf(stderr, "[Worker %d] Received shutdown signal, exiting gracefully\n", thread_id);
  return NULL;
}

// ============================================================================
// Universal Signal Handler Thread
// ============================================================================

void *
signal_handler_thread(void * arg)
{
  sigset_t * mask = (sigset_t *) arg;
  int sig, err;
  int signal_count = 0;

  fprintf(stderr, "[Signal Handler] Thread started\n");
  fprintf(stderr, "[Signal Handler] Ready to catch ALL possible signals\n");
  fprintf(stderr, "[Signal Handler] Send any signal using: kill -<signal> %d\n\n", getpid());

  for (; running;)
    {
      // Wait for ANY signal in the mask
      err = sigwait(mask, &sig);

      if (err != 0)
        {
          fprintf(stderr, "[Signal Handler] sigwait error: %s\n", strerror(err));
          continue;
        }

      signal_count++;

      // Thread-safe logging (we can use mutex because of sigwait!)
      pthread_mutex_lock(&log_mutex);
      print_signal_info(sig);
      pthread_mutex_unlock(&log_mutex);

      // Handle the signal based on its type
      if (is_in_array(sig, shutdown_signals, SHUTDOWN_SIGNALS_COUNT))
        {
          // Graceful shutdown signals
          fprintf(stderr, "[Signal Handler] Initiating graceful shutdown...\n");
          fprintf(stderr, "[Signal Handler] Total signals handled: %d\n", signal_count);
          running = 0;

          // In a real application, you might:
          // - Close network connections
          // - Flush buffers
          // - Save state
          // - Notify other threads

          break; // Exit the signal handler loop
        }
      else if (is_in_array(sig, fatal_signals, FATAL_SIGNALS_COUNT))
        {
          // Fatal signals - we caught it, but we should still exit
          fprintf(stderr, "[Signal Handler] Fatal signal received!\n");
          fprintf(stderr, "[Signal Handler] Dumping diagnostic info and exiting...\n");
          fprintf(stderr, "[Signal Handler] Total signals handled before crash: %d\n", signal_count);

          // You could do cleanup here, but be careful with fatal signals
          // The program state might be corrupted

          // Re-raise the signal to get proper core dump/behavior
          signal(sig, SIG_DFL); // Restore default handler
          raise(sig);           // Re-raise to terminate properly
          // Not reached
        }
      else
        {
          // Informational/ignorable signals
          fprintf(stderr, "[Signal Handler] Signal %d logged and acknowledged\n", sig);
          fprintf(stderr, "[Signal Handler] Continuing normal operation...\n\n");
        }
    }

  fprintf(stderr, "[Signal Handler] Thread exiting\n");
  return NULL;
}

// ============================================================================
// Create a Complete Signal Mask (All Catchable Signals)
// ============================================================================

static void
create_full_signal_mask(sigset_t * mask)
{
  // Start with ALL signals
  sigfillset(mask);

  // Remove signals that CANNOT be blocked or caught
  sigdelset(mask, SIGKILL); // Cannot be caught or blocked
  sigdelset(mask, SIGSTOP); // Cannot be caught or blocked

  // Note: On some systems, you might also want to remove:
  // - SIGCANCEL (used by pthreads internally)
  // - SIGSETXID (used by glibc for setuid/setgid)
  // But these are typically handled automatically
}

// ============================================================================
// Print Available Signals (For Documentation)
// ============================================================================

static void
print_signal_list(void)
{
  fprintf(stderr, "══════════════════════════════════════════════════════════"
                  "═════════════\n");
  fprintf(stderr, "  AVAILABLE SIGNALS ON THIS SYSTEM\n");
  fprintf(stderr, "══════════════════════════════════════════════════════════"
                  "═════════════\n\n");

  fprintf(stderr, "  FATAL SIGNALS (Will cause termination after logging):\n");
  for (size_t i = 0; i < FATAL_SIGNALS_COUNT; i++)
    fprintf(stderr, "    %2d  SIG%-8s  %s\n", fatal_signals[i], strsignal(fatal_signals[i]) ? fatal_signals_str[i] : "",
            strsignal(fatal_signals[i]));

  fprintf(stderr, "\n  SHUTDOWN SIGNALS (Graceful termination):\n");
  for (size_t i = 0; i < SHUTDOWN_SIGNALS_COUNT; i++)
    fprintf(stderr, "    %2d  SIG%-8s  %s\n", shutdown_signals[i],
            strsignal(shutdown_signals[i]) ? shutdown_signals_str[i] : "", strsignal(shutdown_signals[i]));

  fprintf(stderr, "\n  UNCATCHABLE SIGNALS:\n");
  fprintf(stderr, "    %2d  SIGKILL     Kill (cannot be caught)\n", SIGKILL);
  fprintf(stderr, "    %2d  SIGSTOP     Stop (cannot be caught)\n", SIGSTOP);

  fprintf(stderr, "\n════════════════════════════════════════════════════════"
                  "═══════════════\n\n");
}

// ============================================================================
// Main Function
// ============================================================================

int
main(int argc, char * argv[])
{
  pthread_t workers[N_THREADS];
  pthread_t signal_thread;
  sigset_t full_mask;
  int thread_ids[N_THREADS] = { 0 };

  for (int i = 0; i < N_THREADS; i++)
    thread_ids[i] = i;

  fprintf(stderr, "\n");
  fprintf(stderr, "╔═════════════════════════════════════════════════════════"
                  "════════════╗\n");
  fprintf(stderr, "║      UNIVERSAL SIGNAL HANDLER - Multi-threaded Example  "
                  "            ║\n");
  fprintf(stderr, "╚═════════════════════════════════════════════════════════"
                  "════════════╝\n");
  fprintf(stderr, "\n");

  fprintf(stderr, "Process ID: %d\n", getpid());
  fprintf(stderr, "\n");

  // Print signal documentation
  print_signal_list();

  // ========================================================================
  // STEP 1: Create a mask with ALL catchable signals
  // ========================================================================
  create_full_signal_mask(&full_mask);

  // Count how many signals are in the mask
  int signal_count = 0;
  for (int s = 1; s < 65; s++)
    // Signals typically 1-64
    if (sigismember(&full_mask, s))
      signal_count++;

  fprintf(stderr, "[Main] Created signal mask with %d catchable signals\n", signal_count);

  // ========================================================================
  // STEP 2: Block ALL signals in the main thread
  // ========================================================================
  if (pthread_sigmask(SIG_BLOCK, &full_mask, NULL) != 0)
    {
      perror("pthread_sigmask");
      exit(EXIT_FAILURE);
    }
  fprintf(stderr, "[Main] All signals blocked in main thread\n");
  fprintf(stderr, "\n");

  fprintf(stderr, "──────────────────────────────────────────────────────────"
                  "─────────────\n");
  fprintf(stderr, "  TRY THESE COMMANDS IN ANOTHER TERMINAL:\n");
  fprintf(stderr, "──────────────────────────────────────────────────────────"
                  "─────────────\n");
  fprintf(stderr, "  kill -SIGPIPE  %d   # Test pipe signal\n", getpid());
  fprintf(stderr, "  kill -SIGUSR1  %d   # Test user signal 1\n", getpid());
  fprintf(stderr, "  kill -SIGUSR2  %d   # Test user signal 2\n", getpid());
  fprintf(stderr, "  kill -SIGALRM  %d   # Test alarm signal\n", getpid());
  fprintf(stderr, "  kill -SIGTERM  %d   # Graceful shutdown\n", getpid());
  fprintf(stderr, "  kill -SIGINT   %d   # Ctrl+C simulation\n", getpid());
  fprintf(stderr, "  kill -SIGKILL  %d   # Uncatchable - immediate death!\n", getpid());
  fprintf(stderr, "──────────────────────────────────────────────────────────"
                  "─────────────\n");
  fprintf(stderr, "\n");

  // ========================================================================
  // STEP 3: Create worker threads (they inherit the blocked mask)
  // ========================================================================
  for (int i = 0; i < N_THREADS; i++)
    if (pthread_create(&workers[i], NULL, worker_thread, &thread_ids[i]) != 0)
      {
        perror("pthread_create worker");
        exit(EXIT_FAILURE);
      }
  fprintf(stderr, "[Main] Created %d worker threads (signals blocked in all)\n", N_THREADS);

  // ========================================================================
  // STEP 4: Create the dedicated signal handler thread
  // ========================================================================
  if (pthread_create(&signal_thread, NULL, signal_handler_thread, &full_mask) != 0)
    {
      perror("pthread_create signal thread");
      exit(EXIT_FAILURE);
    }
  fprintf(stderr, "[Main] Created signal handler thread\n\n");

  // ========================================================================
  // STEP 5: Main thread waits for all workers
  // ========================================================================
  for (int i = 0; i < N_THREADS; i++)
    pthread_join(workers[i], NULL);

  // Workers are done, signal thread should exit too
  running = 0;
  pthread_join(signal_thread, NULL);

  fprintf(stderr, "\n[Main] All threads terminated cleanly\n");
  fprintf(stderr, "[Main] Goodbye!\n");

  return 0;
}
