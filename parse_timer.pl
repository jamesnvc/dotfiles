#!/usr/bin/env perl
use strict;

my $headers = <>;
my $start = index($headers, "LEFT");
my $end = index($headers, "LAST");
my $line = <>;
my $left = substr($line, $start, $end - $start);
$left =~ s/^\s+//;
$left =~ s/\s+$//;
if ($left ne "n/a") {
  print($left);
} else {
  my $passed_start = index($headers, "PASSED");
  my $passed_end = index($headers, "UNIT");
  my $passed = substr($line, $passed_start, $passed_end - $passed_start);
  print($passed);
}
