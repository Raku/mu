package Pugs::Grammar::Var;
use Pugs::Grammar::Base;
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

sub variable {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    die "not implemented";
    my ($extracted,$remainder) = Text::Balanced::extract_delimited( $_[0], "'" );
    $extracted = substr( $extracted, 1, -1 ) if length($extracted) > 1;
    return Pugs::Runtime::Match->new( { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
    } );
}

1;
