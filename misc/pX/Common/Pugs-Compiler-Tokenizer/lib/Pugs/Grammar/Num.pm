package Pugs::Grammar::Num;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);
use Pugs::Runtime::Match;

=for pod

Parses the text inside strings like:

    1
    1.1
    +1
    1e10
    
=head1 See also

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

our %hash = (
    q() => Pugs::Compiler::Rule->compile( q(
                \d+ 
                { return { num => $() ,} }
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(Inf) => Pugs::Compiler::Rule->compile( q(
                { return { num => 'Inf' ,} }
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(NaN) => Pugs::Compiler::Rule->compile( q(
                { return { num => 'NaN' ,} }
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
);

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

*parse = Pugs::Compiler::Rule->compile( '
    %Pugs::Grammar::Num::hash
    { return Pugs::Grammar::Num::capture( $/ ) }
' )->code;

1;
