use strict;
use File::Basename;
my $Bin; 
BEGIN { $Bin = dirname(__FILE__) };
use lib "$Bin/../..", "$Bin/../../inc";
use inc::Module::Install;

name       ('Pugs-MakeMaker');
version    ('0.01');
abstract   ('MakeMaker for Pugs');
author     ('Brian Ingerson <ingy@cpan.org>');
license    ('perl');
set_blib   (5);
WriteAll   ();
