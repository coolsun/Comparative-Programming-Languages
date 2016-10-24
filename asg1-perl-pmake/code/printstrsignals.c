// $Id: printstrsignals.c,v 1.2 2014-10-10 18:23:38-07 - - $
//
// NAME
//    printstrsignals - print all strsignal numbers and messages
//
// SYNOPSIS
//    printstrsignals
//
// DESCRIPTION
//    Prints all strsignal(3) strings preceded by the signal number.
//    <signal.h>_NSIG is the highest signal number + 1.
//

#define _GNU_SOURCE

#include <libgen.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main (void) {
   for (int signr = 0; signr < _NSIG; ++signr) {
      char *sigmessage = strsignal (signr);
      if (sigmessage != NULL) printf ("%d %s\n", signr, sigmessage);
   }
   return EXIT_SUCCESS;
}
