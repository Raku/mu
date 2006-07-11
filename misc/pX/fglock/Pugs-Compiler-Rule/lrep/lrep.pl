#!/usr/bin/perl
# Internal
require 'p6rule.pl';
require 'emit.pl';
require 'p6prelude-cached.pl';
require 'p6primitives-cached.pl';
use interface;
# External
use IO::File;

# Process command line arguments
my $input_filename = shift || die "Missing arguments!";
my $output_filename = shift || die "Missing arguments!";

# Open the input file
my $input_file = IO::File->new($input_filename,'<') ||
  die 'Could not open the source file '.$input_filename;

# Read the input file
local $/ = undef;
my $source = <$input_file>;

# Compile the input file
my $match = grammar1::grammar->($source);
die 'Syntax error at !!'.$match->{tail}.'!!' if ($match->{tail});
my $code = Perl6Grammar::emit($match->{capture});

# Write down
my $output_file = IO::File->new($output_filename,'>') ||
    die 'Could not open the output file '.$output_filename;
print $output_file $code;

