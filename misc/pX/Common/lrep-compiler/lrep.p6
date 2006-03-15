#!/usr/bin/perl
use warnings;

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
my %args;                       # declaration and assignment in the
%args = ();                     # same line still not working
my $argsref;                    # using verbose code here to simplify rules
$argsref = %args;               # see A02
getopts('i:o:', $argsref);      # this is just like perl 5
my $input_filename;
$input_filename = %args{'i'};   # hash access...
my $output_filename;
$output_filename = %args{'o'};

# test arguments
require Grammar::Perl6Primitives; # missing rules, using hacks
statement_control:<unless> ( $input_filename ) { die usage(); }
statement_control:<unless> ( $output_filename ) { die usage(); }
sub usage {                     # subroutine definition
    return 'use -i input -o output'; # literal return
}

# open file
my $input_file;
$input_file = IO::File.new($input_filename,'<'); # method call
statement_control:<unless> ( $input_file ) # missing rules, using hacks
{ die 'Could not open input file'; }

# read file
my $source;
$source = slurp $input_file;    # slurp is standard in Perl 6


...;                            # yadayada
