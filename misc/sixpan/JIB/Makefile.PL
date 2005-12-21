use ExtUtils::MakeMaker;
use strict;

WriteMakefile (
    NAME            => 'JIB',
    VERSION_FROM    => 'lib/JIB.pm', # finds $VERSION
    ABSTRACT_FROM   => 'lib/JIB.pm',
    dist            => { COMPRESS => 'gzip -9f', SUFFIX => 'gz' },
    PREREQ_PM       => {    'Object::Accessor'      => 0,
                            'Test::More'            => 0,
                            'Params::Check'         => '0.23',
                            'Log::Message::Simple'  => 0,
                            'File::Find'            => 0,
                            'Path::Class'           => 0,
                            'File::Find::Rule'      => 0,
                            'File::chdir'           => 0,
                            'Package::Constants'    => 0,
                            'Module::Load::Conditional' => 0,
                    },
    AUTHOR          => 'Jos Boumans <kane[at]cpan.org>',
);             


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
