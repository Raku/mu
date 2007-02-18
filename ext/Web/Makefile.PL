use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Web');
version    ('0.0.1'); # 'VERSION_FROM' => 'lib/Web.pm'
abstract   ('A general purpose Web toolkit for Perl 6');
author     ('Juerd Waalboer C<juerd@cpan.org>');
license    ('perl');

WritePugs  (6);
