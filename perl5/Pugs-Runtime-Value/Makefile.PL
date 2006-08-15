#!/usr/bin/perl

use strict;
use warnings;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

name          ('Pugs::Runtime::Value');
version_from  ('lib/Perl6/Value/List.pm');
abstract_from ('lib/Perl6/Value/List.pm');
author        ('Flavio S. Glock <fglock@gmail.com>');
WritePugs     (5);
