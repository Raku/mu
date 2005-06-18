use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('HTTP::Server::Lite');
version    ('6.00');
abstract   ('A minimal HTTP server');
author     ('Sebastian Riedel <sri@oook.de>');
license    ('perl');

WritePugs  (6);
