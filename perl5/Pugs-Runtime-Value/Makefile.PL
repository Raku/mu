#!/usr/bin/perl

use strict;
use warnings;
my @inc;
BEGIN {
    @inc = (-e '../../inc' and -e '../../VICTUALS')
        ? (prefix => '../../inc')
        : (-e '../../../../inc' and -e '../../../../VICTUALS')
        ? (prefix => '../../../../inc')
        : ()
}
use lib '../..', '../../../..', (@inc ? $inc[-1] : ());
use inc::Module::Install @inc;

name          ('Pugs-Runtime-Value');
version_from  ('lib/Pugs/Runtime/Value/List.pm');
abstract_from ('lib/Pugs/Runtime/Value/List.pm');
author        ('Flavio S. Glock <fglock@gmail.com>');
requires      ('Moose'  => '0.10');

&WriteAll;
