#!/usr/bin/pugs

use v6;
use Test;

plan 7;

use_ok('Perl::Meta::Role::Behavior');
use_ok('Perl::Meta::Class::Behavior');
use_ok('Perl::Meta::Class');

use_ok('Perl::Meta::Property');
use_ok('Perl::Meta::Method');
use_ok('Perl::Meta::Type');

use_ok('Perl::Meta::Compiler');
