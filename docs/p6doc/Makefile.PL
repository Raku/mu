use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perl6-Doc');
version    (0.01);
license    ('perl');

makemaker_args( PMLIBDIRS => [ grep { -d } glob("[A-Z]*") ]);

WritePugs  (5);
