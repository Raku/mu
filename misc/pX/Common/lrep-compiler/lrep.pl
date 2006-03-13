#!/usr/bin/perl
# Internal
use warnings;
use strict;

require Runtime::Perl5::RuleInit;
require Grammar::Perl6Init;
require Grammar::Perl6;
require Emitter::Perl5;

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

# Parse the input file
my $match = Grammar::Perl6::grammar->($source);
die 'Syntax error at !!'.$match->{tail}.'!!' if ($match->{tail});

# Dump the backend code
my $code = Emitter::Perl5::emit($match->{capture});

# Write down
my $output_file = IO::File->new($output_filename,'>') ||
    die 'Could not open the output file '.$output_filename;
print $output_file $code;

