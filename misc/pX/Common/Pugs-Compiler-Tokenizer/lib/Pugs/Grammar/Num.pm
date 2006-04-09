package Pugs::Grammar::Var;
use Pugs::Grammar::Base;
use Pugs::Runtime::Match;

=for pod

Parses the text inside strings like:

    1
    1.1
    +1
    1e10
    
=head1 See also

=cut

sub num {
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
