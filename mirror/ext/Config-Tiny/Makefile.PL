use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Config::Tiny');
version    ('0.01');
abstract   ('Read/Write .ini style files with as little code as possible');
author     ('Joshua Gatcomb <Limbic_Region_2000@Yahoo.com>');
license    ('perl');

WritePugs  (6);
