use strict;
use lib "../..", "../../inc";
use inc::Module::Install (
    -d '../../misc/pX' ? (prefix => '../../inc') : ()
);
use File::Path;

name       ('Perl6-Doc');
version    (0.01);
license    ('perl');

install_script( 'p6doc' );
makemaker_args( PMLIBDIRS => [ grep { -d } glob("[A-Z]*") ]);

system "$^X Spec/update";

WritePugs  (5);
