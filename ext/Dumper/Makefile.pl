use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Dumper');
version    ('0.001');
abstract   ('Perl 6 Dumper');
author     ('Jonny Schulz <jschulz.cpan@bloonix.de>');
license    ('perl');

WritePugs  (6);
