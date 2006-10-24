package Pugs::Emitter::Perl6::Perl5::Any;

# Note: junctions are *not* Any

use strict;
use warnings;
use base 'Pugs::Emitter::Perl6::Perl5::node';

    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub boxed {
        $_[0]
    }
    sub name {
        $_[0]->{name}
    }
    sub WHAT { 
        $_[0]->node( 'str', 'Any' );
    }
    sub isa { 
        # $_[0]->node( 'bool', 1 );   -- junctions are not Any
        $_[0]->WHAT . ' eq ' . $_[1]->WHAT;
    }

1;

__END__

sub FETCH {
    my $self = $_[0];
    return $self->name;
}

sub STORE {
    my $self = $_[0];
    return $self->name . ' = ' . $_[1]->get;
}

sub str {
    $_[0]->node( 'StrExpression', '( "" . ' . $_[0] . ' )' );
}

sub perl {
    $_[0]->node( 'str', "$_[0]" )
}
    
sub defined {
    'defined ' . $_[0];
}

sub print {
    $_[0]->node( 'AnyExpression', '( print "", ' . $_[0]->str . ' )' );
}

sub say {
    $_[0]->node( 'AnyExpression', '( print "", ' . $_[0]->str . ', "\n" )' );
}

sub warn {
    $_[0]->node( 'AnyExpression', '( warn ' . $_[0]->str . ' )' );
}

sub yaml {
    $_[0]->node( 'StrExpression', 'Pugs::Runtime::Perl6::Scalar::yaml( ' . $_[0] . ' )' );
}

