use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Regexp::Engine::Reentrant',
    AUTHOR              => 'Pugs Team <perl6-compiler@perl.org>',
    VERSION_FROM        => 'lib/Regexp/Engine/Reentrant.pm',
    ABSTRACT_FROM       => 'lib/Regexp/Engine/Reentrant.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'version'    => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Regexp-Engine-Reentrant-*' },
);
