use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('DateTime::Set');
version    ('0.01');
abstract   ('DateTime Span sets and recurrences');
author     ('Flavio S. Glock');
license    ('perl');

WritePugs  (6);
