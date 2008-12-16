use 5.010000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Syntax::Highlight::Perl6',
    VERSION_FROM      => 'lib/Syntax/Highlight/Perl6.pm', # finds $VERSION
    PREREQ_PM         => {
        'Moose'               => 0,        
        'Readonly'            => 0,
        'Term::ANSIColor'     => 0,
        'YAML::Syck'          => 0
    },
    EXE_FILES         => ['bin/syntax-highlight-p6'],
    LICENSE           => 'artistic_2',
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Syntax/Highlight/Perl6.pm', # retrieve abstract from module
       AUTHOR         => 'Ahmad M. Zawawi <ahmad.zawawi@gmail.com>') : ()),
);
