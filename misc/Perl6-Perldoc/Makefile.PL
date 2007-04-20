use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Perl6::Perldoc',
    AUTHOR              => 'Damian Conway <DCONWAY@CPAN.org>',
    VERSION_FROM        => 'lib/Perl6/Perldoc.pm',
    ABSTRACT_FROM       => 'lib/Perl6/Perldoc.pm',
    EXE_FILES           => ['bin/perldoc2text','bin/perldoc2xhtml'],
    PL_FILES            => {},
    PREREQ_PM => {
        'Filter::Simple' => 0,
        'Test::More'   => 0,
        'version'      => 0,
        'Scalar::Util' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Perl6-Perldoc-*' },
);
