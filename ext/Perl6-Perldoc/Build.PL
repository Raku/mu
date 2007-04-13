use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Perl6::Perldoc',
    license             => 'perl',
    dist_author         => 'Damian Conway <DCONWAY@CPAN.org>',
    dist_version_from   => 'lib/Perl6/Perldoc.pm',
    requires => {
        'Test::More'   => 0,
        'version'      => 0,
        'Scalar::Util' => 0,
    },
    add_to_cleanup      => [ 'Perl6-Perldoc-*' ],
);

$builder->create_build_script();
