
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Perl6::Container::Array',
    VERSION_FROM      => 'lib/Perl6/Container/Array.pm', # finds $VERSION
    PREREQ_PM         => { 'Perl6::Value::List' => 0 }, 
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/Perl6/Container/Array.pm', # retrieve abstract from module
       AUTHOR         => 'Flavio S. Glock <fglock@gmail.com>') : ()),
);
