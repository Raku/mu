use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Algorithm::TokenBucket');
version    ('0.2');
abstract   ('Token bucket rate limiting algorithm');
author     ('Ingo Blechschmidt <iblech@web.de>');
license    ('perl');

WritePugs  (6);
