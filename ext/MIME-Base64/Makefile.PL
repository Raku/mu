use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('MIME-Base64');
version    ('0.01');
abstract   ('Encoding and decoding of base64 strings');
author     ('David Formosa');
license    ('perl');

WritePugs  (6);
