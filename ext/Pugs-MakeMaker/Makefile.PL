use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Pugs-MakeMaker');
version    ('0.01');
abstract   ('MakeMaker for Pugs');
author     ('Brian Ingerson <ingy@cpan.org>');
license    ('perl');
set_blib   (5);
WriteAll   ();
