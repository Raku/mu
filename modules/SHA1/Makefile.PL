use strict;
use Perl6::MakeMaker;

WriteMakefile(
    NAME    => 'SHA1',
    VERSION => '1.00',
    AUTHOR  => 'Brian Ingerson',
    inline  => 'lib/SHA1.pm',
    clean   => {FILES => 'src/*.hi src/*.o *.hs *.hi *.o'},
);
