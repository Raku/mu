use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Set');
version    ('0.01');
abstract   ('Set Perl6 extension');
author     ('Sam "mugwump" Vilain');
license    ('perl');

WritePugs  (6);

#!pugs
#use v6;
#
#require Perl6::MakeMaker;
#
#WriteMakefile(
#    'NAME' => 'Set',
#    'VERSION_FROM' => 'lib/Set.pm',
#    'PREREQ_PM' => {
#        'Test' => '0.0.2', # but only for the test suite
#    },
#);
#
#1;
