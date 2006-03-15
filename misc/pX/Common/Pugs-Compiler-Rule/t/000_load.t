#!/usr/bin/perl 

use strict;
use warnings;

use Test::More no_plan => 1;

BEGIN {
	use_ok('Pugs::Compiler::Rule');
	use_ok('Pugs::Emitter::Rule::Perl5');	
	use_ok('Pugs::Grammar::Rule');					
	use_ok('Pugs::Runtime::Match');				
	use_ok('Pugs::Runtime::Rule');				
	use_ok('Pugs::Runtime::Rule2');						
}