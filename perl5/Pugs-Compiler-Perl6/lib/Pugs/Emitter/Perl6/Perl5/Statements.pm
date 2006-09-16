use strict;
use warnings;

package Pugs::Emitter::Perl6::Perl5::Statements;
use Data::Dumper;

sub new {
    my @statements = map { Pugs::Emitter::Perl6::Perl5::_emit( $_ ) }  
            @{$_[1]}; 
    #print "Statements: ", Dumper( \@statements );
    my $self = \@statements;  # [ @things ]
    bless $self, $_[0];
    return $self;
}

sub perl {
    #print "Statements: ", Dumper( @{$_[0]} );
    my $statements = join ( ";\n", 
        map { defined $_ ? $_->perl : () } 
        @{$_[0]}, undef 
    );
    #print "Statements: $statements\n";
    return length $statements ? $statements : "";
}

1;
