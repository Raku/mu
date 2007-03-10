use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Regexp::Engine::Reentrant',
    license             => 'perl',
    dist_author         => 'Pugs Team <perl6-compiler@perl.org>',
    dist_version_from   => 'lib/Regexp/Engine/Reentrant.pm',
    requires => {
        'Test::More' => 0,
        'version'    => 0,
    },
    add_to_cleanup      => [ 'Regexp-Engine-Reentrant-*' ],
);

$builder->create_build_script();
