package Pugs::Emitter::Perl6::YAML;

# p6-ast to perl5 emitter

use strict;
use warnings;

use YAML::Syck;

#use Data::Dumper;
#$Data::Dumper::Indent = 1;

#use Pugs::Emitter::Rule::Perl5::Ratchet;
#use Pugs::Runtime::Common;
use Pugs::Runtime::Perl6; 

sub emit {    
    my ($grammar, $ast) = @_;
    return Pugs::Runtime::Perl6::Scalar::yaml( $ast );
}

1;
