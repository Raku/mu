package Pugs::Grammar::Str;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Text::Balanced; 

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
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(") => Pugs::Compiler::Rule->compile( q(
                [ . | \\ \" ]* 
                \"
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
);

*parse = Pugs::Compiler::Rule->compile( '
    <single_quote> | <double_quote>
' )->code;

sub single_quote {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    } );
}

sub double_quote {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], '"' );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    } );
}

1;
