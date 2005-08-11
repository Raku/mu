use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perl::MetaClass');
version    ('0.0.1');
abstract   ('Class System for Perl 6');
author     ('Stevan Little <stevan@iinteractive.com>, Sam Vilain <samv@cpan.org>');
license    ('perl');

WritePugs  (6);

