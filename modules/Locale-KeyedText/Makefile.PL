#!pugs
use v6;

require Perl6::MakeMaker;

WriteMakefile(
	'NAME' => 'Locale::KeyedText',
	'VERSION_FROM' => 'lib/Locale/KeyedText.pm',
	'PREREQ_PM' => {
		'Test' => '0.0.2', # but only for the test suite
	},
);

1;
