package Pugs::Grammar::Num;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
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

*parse = Pugs::Compiler::Rule->compile( '
    <num>
' )->code;

*num = Pugs::Compiler::Rule->compile( '
    \d+
' )->code;

1;
