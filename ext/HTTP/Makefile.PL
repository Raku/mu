use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('HTTP');
version    ('0.0.1'); # 'VERSION_FROM' => 'lib/HTTP.pm'
abstract   ('A general purpose HTTP toolkit for Perl 6');
author     ('Juerd Waalboer C<juerd@cpan.org>');
license    ('perl');

WritePugs  (6);
