use 5.006;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Regexp::Parser',
    VERSION_FROM      => 'lib/Regexp/Parser.pm', # finds $VERSION
    PREREQ_PM         => { NEXT => '' }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Regexp/Parser.pm', # retrieve abstract from module
       AUTHOR         => 'Jeff japhy Pinyan <japhy@perlmonk.org>') : ()),
);
