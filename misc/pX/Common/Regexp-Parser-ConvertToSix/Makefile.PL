use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Regexp::Parser::ConvertToSix',
    AUTHOR              => 'Pugs Team <perl6-compiler@perl.org>',
    VERSION_FROM        => 'lib/Regexp/Parser/ConvertToSix.pm',
    ABSTRACT_FROM       => 'lib/Regexp/Parser/ConvertToSix.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'version'    => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Regexp-Parser-ConvertToSix-*' },
);
