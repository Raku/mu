require 5.000;
use ExtUtils::MakeMaker;
WriteMakefile(
	'NAME'         => 'Algorithm::Dependency',
	'VERSION_FROM' => 'lib/Algorithm/Dependency.pm',
	'PREREQ_PM'    => {
		# Modules needed for tests
		'Test::More'       => '0',
		'Test::ClassAPI'   => '0.6',
		'File::Spec'       => '0.83',

		# Explicitly list the Test::ClassAPI dependencies,
		# because some smoke testers arn't installing them properly.
		'Class::Inspector' => '1.06',
		'Config::Tiny'     => '2.0',
	},
	($] >= 5.005) ? (
		'AUTHOR'     => 'Adam Kennedy (cpan@ali.as)',
		'ABSTRACT'   => 'Create extensible and optimized dependency algorithms',
	) : (),
);
