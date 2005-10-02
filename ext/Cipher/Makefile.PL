use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Cipher');
version    ('0.01');
abstract   ('Perl 6 Cipher API');
author     ('Brent Royal-Gordon <brentdax@cpan.org>');
license    ('perl');

WritePugs  (6);
