#!﻿usr/bin/pugs

use v6;
use Test;

=pod

Tests for the C<enum> function.

=cut

my %hash;

lives_ok { %hash = enum <<:Sun(1) :Mon(2) :Tue(3) :Wed(4) :Thu(5) :Fri(6) :Sat(7)>>; }, 'specifying keys and values works...', :todo<feature>;

is %hash.keys, <Sun Mon Tue Wed Thu Fri Sat>, '...and the right keys are assigned', :todo<feature>;

is %hash<Sun Mon Tue Wed Thu Fri Sat>, 1..7, '...and the right values are assigned', :todo<feature>;

%hash = ();

lives_ok { %hash = enum <<:Sun(1) Mon Tue Wed Thu Fri Sat>>; }, 'specifying a value for only the first key works...', :todo<feature>;

is %hash.keys, <Sun Mon Tue Wed Thu Fri Sat>, '...and the right keys are assigned', :todo<feature>;

is %hash<Sun Mon Tue Wed Thu Fri Sat>, 1..7, '...and the right values are assigned', :todo<feature>;

%hash = ();

lives_ok { %hash = enum «:Sun(1) Mon Tue Wed Thu Fri Sat»; }, 'french quotes work...', :todo<feature>;

is %hash.keys, <Sun Mon Tue Wed Thu Fri Sat>, '...and the right keys are assigned', :todo<feature>;

is %hash<Sun Mon Tue Wed Thu Fri Sat>, 1..7, '...and the right values are assigned', :todo<feature>;

%hash = ();

lives_ok { %hash = enum <<:Sun(1) Mon Tue :Wed(4) Thu Fri Sat>>; }, 'specifying continuous values in the middle works...', :todo<feature>;

is %hash.keys, <Sun Mon Tue Wed Thu Fri Sat>, '...and the right keys are assigned', :todo<feature>;

is %hash<Sun Mon Tue Wed Thu Fri Sat>, 1..7, '...and the right values are assigned', :todo<feature>;

%hash = ();

lives_ok { %hash = enum <<:Sun(1) Mon Tue :Wed(5) Thu Fri Sat>>; }, 'specifying different values in the middle works...', :todo<feature>;

is %hash.keys, <Sun Mon Tue Wed Thu Fri Sat>, '...and the right keys are assigned', :todo<feature>;

is %hash<Sun Mon Tue Wed Thu Fri Sat>, (1, 2, 3, 5, 6, 7, 8), '...and the right values are assigned', :todo<feature>;

%hash = ();
