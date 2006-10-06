package Pugs::Emitter::Perl6::Perl5::Perl5Range;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { 
            '(' . join( ' .. ', @{$_[0]->{name}} ) . ')' 
        },
        fallback => 1,
    );

    sub WHAT { 
        $_[0]->node( 'str', 'Range' );
    }
    
1;


