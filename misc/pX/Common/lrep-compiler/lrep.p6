#use v6; # still need to implement something with this;
use warnings;

# External
use IO::File;
use Getopt::Std;
use Data::Dumper;

# Internal
# There is some loading precedence.
# That's why require is used instead of
# plain use.
require Runtime::Perl5::RuleInit;
require Grammar::Perl6Init;
require Grammar::Rules;
require Grammar::Perl6;
require Emitter::Perl5;

# Process command line arguments
my %args;                       # declaration and assignment in the
%args = ();                     # same line still not working
my $argsref;                    # using verbose code here to simplify rules
$argsref = %args;               # see A02
getopts('ti:o:', $argsref);      # this is just like perl 5
my $input_filename;
$input_filename = %args{'i'};   # hash access...
my $output_filename;
$output_filename = %args{'o'};
$::trace = %args{'t'};			# global variable

# test arguments
unless ( $input_filename ) {
	die usage();
}
unless ( $output_filename ) {
	die usage();
}
sub usage {                     # subroutine definition
    return 'use -i input -o output'; # literal return
}

# open file
my $input_file;
$input_file = IO::File.new($input_filename,'<'); # method call
unless ( $input_file ) {
	die 'Could not open input file';
}

# read file
my $source;
$source = slurp $input_file;    # slurp is standard in Perl 6

# parse file
# XXX - TODO - Grammar should be accessed via methods
# and not via subroutines. This should be actually Perl 6
# rules applying... But for now... this is ok...
my $match;
$match = Grammar::Perl6::grammar($source);

# if something didn't match
my $tail;
$tail = $match{'tail'};
if ( $tail ) {
	say 'Syntax Error !!';
	die $tail;
}

# emit
my $code;
my $capture;
$capture = $match{'capture'};

Data::Dumper.Dump($capture);

$code = Emitter::Perl5::emit($capture);
my $out_fh;
$out_fh = open $output_filename;

print $out_fh: $code;
