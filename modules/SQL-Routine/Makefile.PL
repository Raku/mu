#!perl

use 5.008001; use utf8; use strict; use warnings;

use Pugs::MakeMaker;

WriteMakefile(
	NAME	=> 'SQL::Routine',
    VERSION => '0.0.2',
	PREREQ_PM => {
		'Locale::KeyedText' => '0.0.4',
	},
);

1;
