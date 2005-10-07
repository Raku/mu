use strict;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name      ('XML-SAX');
version   ('0.01');
abstract  ('Simple API for XML');
author    ('Florian Ragwitz <florian.ragwitz@s2004.tu-chemnitz.de>');
license   ('perl');

WritePugs (6);
