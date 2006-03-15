#!/usr/bin/perl
use warnings;
#use strict;

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

$input_filename = %args{'i'};
$output_filename = %args{'o'};

require Grammar::Perl6Primitives;

statement_control:<unless> ( $input_filename ) { die usage(); }
statement_control:<unless> ( $output_filename ) { die usage(); }

sub usage {
    return 'use -i input -o output';
}

my $input_file;
$input_file = IO::File.new($input_filename,'<');
statement_control:<unless> ( $input_file ) { die 'Could not open input file'; }

my $source;
$source = slurp $input_file;


...;
