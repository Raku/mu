use strict;
use warnings;

our ($Class, $Object);

{
	tests => 2,
	code => sub {
		my $Autoloading = $Class->new(
			'$:name'		=> 'Class::Autoloading',
			'$:superclass'	=> $Class,
			'%:methods'		=> {
				'dispatch_method' => sub {
					my $class = shift;
					my $label = shift;

					if (my $method = $class->find_method($label)){
						goto &$method;
					}

					if (my $autoloader = $class->find_method("AUTOLOADER")){
						unshift @_, $label;
						goto &$autoloader;
					}

					Carp::confess "Neither $label nor AUTOLOADER were found";
				},
			},
		);

		my $MyAuto = $Autoloading->new(
			'$:name'		=> 'My::Autoloading::Class',
			'$:superclass'	=> $Object,
			'%:methods'		=> {
				some_method => sub { "some_method" },
				AUTOLOADER  => sub { "autoload_$_[0]" }
			},
		);

		my $iAuto = $MyAuto->new;
		is($iAuto->some_method, "some_method", "successsful dispatch for autoloading class");
		is($iAuto->other, "autoload_other", "autoloader kicked in for missing method in autoloading class");
	}
}
