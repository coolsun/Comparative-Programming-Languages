#!/usr/bin/perl
# $Id: older.perl,v 1.1 2014-10-10 15:23:29-07 - - $
#
# NAME
#    older.perl - check whether a pair of files are older or newer
#
# SYNOPSIS
#    older.perl file1 file2
#
# DESCRIPTION
#    The two files' modification times are compared and a
#    relationship is printed.
#

use strict;
use warnings;
use POSIX qw(strftime);
$0 =~ s|.*/||;

sub mtime ($) {
   my ($filename) = @_;
   my @stat = stat $filename;
   return @stat ? $stat[9] : undef;
}

sub fileinfo ($) {
   my ($filename) = @_;
   my $mtime = mtime $filename;
   print "$filename: ";
   if (defined $mtime) {print strftime "%c\n", localtime $mtime}
                  else {print "$!\n"}
   return $mtime;
}

die "Usage: $0 file1 file2\n" unless @ARGV == 2;

my @mtimes = map {fileinfo $_} @ARGV;

if ((grep {defined $_} @mtimes) == 2) {
   print "$ARGV[0] ($mtimes[0]) ";
   print $mtimes[0] < $mtimes[1] ? "is older than"
       : $mtimes[0] > $mtimes[1] ? "is newer than"
       : "same time as";
   print " $ARGV[1] ($mtimes[1])\n";
}

# Sample output:
# -bash-55$ older.perl /dev/null eratosthenes.perl
# /dev/null: Mon 22 Sep 2014 01:29:47 PM PDT
# eratosthenes.perl: Fri 08 Aug 2014 05:05:59 PM PDT
# /dev/null (1411417787) is newer than eratosthenes.perl (1407542759)
# -bash-56$ older.perl eratosthenes.perl older.perl 
# eratosthenes.perl: Fri 08 Aug 2014 05:05:59 PM PDT
# older.perl: Fri 10 Oct 2014 03:25:44 PM PDT
# eratosthenes.perl (1407542759) is older than older.perl (1412979944)

