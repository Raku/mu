package Pugs::Grammar::Var;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Text::Balanced; 

=for pod

Parses the text inside strings like:

    $a
    @a
    %a
    &a
    
and maybe subscripts, dereferences, and method calls

    @baz[3](1,2,3){$xyz}<blurfl>.attr()

and maybe

    \(...)
    ^T

=head1 See also

=cut

# TODO - implement the "magic hash" dispatcher

*parse = Pugs::Compiler::Rule->compile( '
    <variable>
' )->code;

# copied from PCR Rule.pmc
sub variable {
    my $grammar = shift;
    $_[0] = "" unless defined $_[0];
    my $bool = $_[0] =~ /^([\$\%\@](?:(?:\:\:)?[_[:alnum:]]+)+)(.*)$/sx;
    return {
        bool  => $bool,
        match => $1,
        tail  => $2,
    }
};

1;
