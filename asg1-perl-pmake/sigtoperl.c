// $Id: sigtoperl.c,v 1.4 2011-12-20 17:08:31-08 - - $

#define _GNU_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (void) {
   int sig;
   system ("echo '#' `uname -srp`");
   printf ("my %%strsignal = (\n");
   for (sig = 0; sig < _NSIG; ++sig) {
      char *strsig = strsignal (sig);
      if (strsig == NULL) continue;
      printf ("%5d => \"%s\",\n", sig, strsig);
   }
   printf (");\n");
   return EXIT_SUCCESS;
}

