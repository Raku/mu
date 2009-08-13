use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Inline::Mildew',
    AUTHOR              => q{Paweł Murias <pawelmurias@gmail.com>},
    VERSION_FROM        => 'lib/Inline/Mildew.pm',
    ABSTRACT_FROM       => 'lib/Inline/Mildew.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
	'Inline'     => 
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Inline-Mildew-*' },
);
