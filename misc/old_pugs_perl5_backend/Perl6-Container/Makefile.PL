#!/usr/bin/perl

use strict;
use warnings;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name          ('Perl6-Container-Array');
version_from  ('lib/Perl6/Container/Array.pm');
abstract_from ('lib/Perl6/Container/Array.pm');
author        ('Flavio S. Glock <fglock@gmail.com>');
WritePugs     (5);
