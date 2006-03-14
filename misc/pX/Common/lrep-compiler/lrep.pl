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
%args = ();
getopts('o:i:', \%args);
my $input_filename = $args{i};
if (! $input_filename) {
	die usage();
}
my $output_filename = $args{o};
if (! $output_filename) {
	die usage();
}

sub usage {
	return "Usage: $0 -i inputfile -o outputfile\n";
}

# Open the input file
my $input_file = IO::File->new($input_filename,'<') ||
  die 'Could not open the source file '.$input_filename;

# Read the input file
local $/ = undef;
my $source = <$input_file>;

# Parse the input file
my $match = Grammar::Perl6::grammar->($source);
if ($match->{tail}) {
	my $error = $match->{tail};
	$error =~ s/^(.{1,150}).+$/$1.../s;
	die 'Syntax error at !!'.$error;
}

# Dump the backend code
my $code = Emitter::Perl5::emit($match->{capture});

# Write down
my $output_file = IO::File->new($output_filename,'>') ||
    die 'Could not open the output file '.$output_filename;
print $output_file $code;

