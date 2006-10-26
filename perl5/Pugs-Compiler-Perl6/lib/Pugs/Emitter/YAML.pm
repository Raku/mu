package Pugs::Emitter::YAML;

# p6-ast to perl5 emitter

use strict;
use warnings;

use Pugs::Runtime::Perl6; 

sub emit {    
    my ($grammar, $ast) = @_;
    return Pugs::Runtime::Perl6::Scalar::yaml( $ast );
}

1;
