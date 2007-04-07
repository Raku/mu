use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

warn q{
---------------------------------------------------------------------------
  NOTE: Please ignore any warnings during installation about the use of
  "=encoding utf8" in the pod documentation.

  Continuing in 3 seconds...
---------------------------------------------------------------------------
};
sleep 3;

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'QDRDBMS',
    'VERSION_FROM' => 'lib/QDRDBMS.pm', # finds $VERSION
    'ABSTRACT' => 'A fully-featured truly relational DBMS in Perl',
    'AUTHOR' => 'Darren Duncan <perl@DarrenDuncan.net>',
#    'LICENSE' => 'gpl',
    'PREREQ_PM' => {
    },
);

1; # Magic true value required at end of a reuseable file's code.
