use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Net::IRC');
version    ('0.01');
abstract   ('Net::IRC for Pugs');
author     ('Ingo Blechschmidt <iblech@web.de>');
license    ('perl');

WritePugs  (6);
