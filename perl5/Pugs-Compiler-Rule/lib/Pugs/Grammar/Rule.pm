# Pugs::Grammar::Rule - fglock
#
# the 'Rule' grammar - 'rule' is the main rule
#

package Pugs::Grammar::Rule;
use strict;
use warnings;
no  warnings qw( once redefine uninitialized );

use Text::Balanced; 
use Data::Dumper;
use Pugs::Runtime::LrepRule;
use Pugs::Runtime::LrepMatch;
#use Pugs::Runtime::Grammar; -- MOP 

use vars qw( @rule_terms );
use base 'Pugs::Grammar::LrepBase';
use Pugs::Grammar::Rule::Rule;   # compiled with lrep


1;
