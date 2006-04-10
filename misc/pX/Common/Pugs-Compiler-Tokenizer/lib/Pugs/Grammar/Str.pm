package Pugs::Grammar::Str;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);
#use Pugs::Runtime::Match;
#use Text::Balanced; 

=for pod

Parses the text inside strings like:

    '...' 
    "..."
    q(...)
    qq(...)
    qw(...)
    <... ...>
    <<... ...>>
    «...»

Quoting constructs are macros:

    macro quote:<qX> (*%adverbs) {...}

=head1 See also

S02

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

our %hash = (
    q(') => Pugs::Compiler::Rule->compile( q(
                [ . | \\ \' ]* 
                \'
                { return { single_quoted => "\'" . $() ,} }
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(") => Pugs::Compiler::Rule->compile( q(
                [ . | \\ \" ]* 
                \"
                { return { double_quoted => "\"" . $() ,} }
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
);

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

*parse = Pugs::Compiler::Rule->compile( q(
    %Pugs::Grammar::Str::hash
    { return Pugs::Grammar::Str::capture( $/ ) }
) )->code;

1;
