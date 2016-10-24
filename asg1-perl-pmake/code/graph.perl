#!/usr/bin/perl
# $Id: graph.perl,v 1.4 2014-10-10 15:54:49-07 - - $

use strict;
use warnings;
$0 =~ s|.*/||;

# Example setting up a directed graph.

my @inputs = (
   "all : hello",
   "hello : main.o hello.o",
   "main.o : main.c hello.h",
   "hello.o : hello.c hello.h",
   "ci : Makefile main.c hello.c hello.h",
   "test : hello",
   "clean : ",
   "spotless : clean",
);

sub parse_dep ($) {
   my ($line) = @_;
   return undef unless $line =~ m/^(\S+)\s*:\s*(.*?)\s*$/;
   my ($target, $dependency) = ($1, $2);
   my @dependencies = split m/\s+/, $dependency;
   return $target, \@dependencies;
}

my %graph;
for my $input (@inputs) {
   my ($target, $deps) = parse_dep $input;
   print "$0: syntax error: $input\n" and next unless defined $target;
   $graph{$target} = $deps;
}

for my $target (keys %graph) {
   print "\"$target\"";
   my $deps = $graph{$target};
   if (not @$deps) {
      print " has no dependencies";
   }else {
      print " depends on";
      print " \"$_\"" for @$deps;
   }
   print "\n";
}

# Sample output:
# "test" depends on "hello"
# "clean" has no dependencies
# "all" depends on "hello"
# "main.o" depends on "main.c" "hello.h"
# "ci" depends on "Makefile" "main.c" "hello.c" "hello.h"
# "hello.o" depends on "hello.c" "hello.h"
# "spotless" depends on "clean"
# "hello" depends on "main.o" "hello.o"
