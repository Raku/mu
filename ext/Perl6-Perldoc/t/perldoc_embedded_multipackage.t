#! /usr/bin/perl

use Perl6::Perldoc;
use strict;
use warnings;
use Test::More tests=>3;

=for DATA
main data

=for DATA
more main data

package Other;

=for DATA
other data

=for DATA
more other data

package main;

=for DATA
still more main data

::is_deeply \@DATA, [ "main data\n",
                      "more main data\n",
                      "other data\n",
                      "more other data\n",
                      "still more main data\n", ] => '*main::DATA correct';

package Other;

::is_deeply \@DATA, [ "main data\n",
                      "more main data\n",
                      "other data\n",
                      "more other data\n",
                      "still more main data\n", ] => '*Other::DATA correct';

package Elsewhere;

::is_deeply \@DATA, [ "main data\n",
                      "more main data\n",
                      "other data\n",
                      "more other data\n",
                      "still more main data\n", ] => '*Elsewhere::DATA correct';
