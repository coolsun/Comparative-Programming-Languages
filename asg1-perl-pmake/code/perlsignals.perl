#!/usr/bin/perl
# $Id: perlsignals.perl,v 1.2 2011-12-19 20:00:05-08 - - $
#
# Illustration of how to load signal messages from a C program.
# The Perl library lacks strsignal(3).  It is assumed that the
# executable binary compikled from C is in the same directory as
# the Perl program itself.
#

use strict;
use warnings;

my $allsignals = $0;
$0 =~ s|.*/||;
$allsignals =~ s|$0$|allsignals|;

print "allsignals=$allsignals\n";
my @strsignals;
map {$_ =~ m/(\d+)\s+(.*)/ and $strsignals[$1] = $2} `$allsignals`;

for my $signr (0..$#strsignals) {
   print "$signr $strsignals[$signr]\n" if defined $strsignals[$signr];
}

