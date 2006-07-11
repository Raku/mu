package interface;
our $print_program = 0;
our $print_ast     = 0;
our $print_match   = 0;
our $pretty_print  = 0; #no effect yet...
our $format        = 'p5';
our $got_options   = 0;
sub import {
	return if $got_options;
	$got_options = 1;
	eval {require Getopt::Long;import Getopt::Long};
	return if $@;

	##print "* reading command line options\n";
	GetOptions(
		"print-program" => \$print_program,
		"print-ast" => \$print_ast,
		"print-match" => \$print_match,
		"pretty-print" => \$pretty_print,
		"format=s" => \$format
	);
}
1;

__END__

our @prelude;    => The files that make up the prelude
"prelude=s@" => \@prelude

