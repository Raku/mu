use strict;
use File::Basename;
my $Bin; 
BEGIN { $Bin = dirname(__FILE__) };
use lib "$Bin/../..", "$Bin/../../inc";
use inc::Module::Install;

name       ('Test');
version    ('0.01');
abstract   ('Test Module');
author     ('Autrijus Tang <autrijus@cpan.org>');
license    ('perl');

set_blib   (6);
WritePugs  ();
