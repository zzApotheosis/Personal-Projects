//#include <unistd.h>

#ifdef SLIST_EXAMPLE
#include "slist.h"
#endif

#ifdef STAILQ_EXAMPLE
#include "stailq.h"
#endif

/* Main function */
int main(int argc, char * argv[]) {
#ifdef SLIST_EXAMPLE
  do_slist_example(argc, argv);
#endif
#ifdef STAILQ_EXAMPLE
  do_stailq_example(argc, argv);
#endif
}
