#!/usr/bin/perl
use warnings;
use strict;

# External
use IO::File;
use Getopt::Std;

# Internal
# There is some loading precedence.
# That's why require is used instead of
# plain use.
require Runtime::Perl5::RuleInit;
require Grammar::Perl6Init;
require Grammar::Perl6;
require Emitter::Perl5;

# Process command line arguments
my %args;
# declaration and assignment in the same
# line still not working
%args = ();
my $input_filename;
my $output_filename;

# reference creation
# see Apocalypse 2
my $argsref;
$argsref = %args;

getopts('i:o:', $argsref);

...;
