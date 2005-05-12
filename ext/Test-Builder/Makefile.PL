use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Test-Builder');
version    ('0.1.0');
abstract   ('Test::Builder for Perl 6');
author     ('chromatic <chromatic@cpan.org>');
license    ('perl');

WritePugs  (6);
