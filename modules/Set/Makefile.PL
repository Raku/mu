
#!pugs
use v6;

require Perl6::MakeMaker;

WriteMakefile(
	'NAME' => 'Set',
	'VERSION_FROM' => 'lib/Set.pm',
	'PREREQ_PM' => {
		'Test' => '0.0.2', # but only for the test suite
	},
);

1;
