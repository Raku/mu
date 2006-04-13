package Pugs::Grammar::Str;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

=for pod

Parses text like:

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

BEGIN {
    __PACKAGE__->add_rule( q(') => q(
                [ . | \\ \' ]* 
                \'
                { return { single_quoted => "\'" . $() ,} }
            ) );
    __PACKAGE__->add_rule( q(") => q(
                [ . | \\ \" ]* 
                \"
                { return { double_quoted => "\"" . $() ,} }
            ) );
    __PACKAGE__->recompile;
}

1;
