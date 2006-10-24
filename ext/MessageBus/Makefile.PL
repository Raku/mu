use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('MessageBus');
version    ('0.01');
abstract   ('Message Bus with PubSub protocol');
author     ('Audrey Tang');
license    ('perl');

WritePugs  (6);
