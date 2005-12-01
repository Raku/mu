use ExtUtils::MakeMaker;
use strict;

WriteMakefile (
    NAME            => 'JIB',
    VERSION_FROM    => 'lib/JIB.pm', # finds $VERSION
    dist            => { COMPRESS => 'gzip -9f', SUFFIX => 'gz' },
    PREREQ_PM       => {    'Object::Accessor'      => 0,
                            'Test::More'            => 0,
                            'Params::Check'         => '0.23',
                            'Log::Message::Simple'  => 0,
                    },
    AUTHOR          => 'Jos Boumans <kane[at]cpan.org>',
	ABSTRACT        => 'Perl6 Package Manager'
);             


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
