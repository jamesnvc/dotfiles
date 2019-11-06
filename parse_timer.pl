#!/usr/bin/env perl
use strict;

my $headers = <>;
my $start = index($headers, "LEFT");
my $end = index($headers, "LAST");
my $line = <>;
print substr($line, $start, $end - $start);
