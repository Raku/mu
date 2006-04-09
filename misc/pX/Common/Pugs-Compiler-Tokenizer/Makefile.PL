#!/usr/bin/perl

use strict;
use warnings;
use lib '../../../..', '../../../../inc';
use inc::Module::Install prefix => '../../../../inc';

name          ('Pugs-Compiler-Tokenizer');
version_from  ('lib/Pugs/Grammar/Var.pm');
abstract_from ('lib/Pugs/Grammar/Var.pm');
author        ('Flavio S. Glock <fglock@gmail.com>');
WritePugs     (5);
