
package Match;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '""'     => \&flat, # XXX - wrong - should use "result object"
    '0+'     => \&flat, # XXX - wrong - should use "result object"
    fallback => 1,
);

sub _str {
    my $match = $_[0];
    my $ref = ref( $match );
    # use Data::Dumper;
    # print "MATCH: ", Dumper( $match ), "\n";
    return $match unless $ref;
    if ( $ref eq 'HASH' ) {
        if ( exists $match->{match} ) {
            if ( ref( $match->{match} ) eq 'ARRAY' ) {
                return join( '', map { _str( $_ ) } @{$match->{match}} );
            }
            if ( ref( $match->{match} ) eq 'HASH' ) {
                return join( '', map { _str( $_ ) } values %{$match->{match}} );
            }
        }
        return join( '', map { _str( $_ ) } values %{$match} );
    }
    if ( $ref eq 'ARRAY' ) {
        return join( '', map { _str( $_ ) } @{$match} );
    }
    #        return join( '', values %{$match->{match}} )
    #            unless exists $match->{match}{match};
    die 'not a match';
}

sub new {
    my ($class, $match) = @_;
    my $self = \$match;
    bless $self, $class;
}

sub flat {
    _str(${$_[0]});
    # join '', map { values %{$_->{match}} } @{${$_[0]}->{match}};
}

# return the capture
sub code {
    my $c = ${$_[0]}->{capture};
    return sub { $c };
}

# return the capture
sub capture {
    ${$_[0]}->{capture};
}

# return the bool value
sub bool {
    ${$_[0]}->{bool};
}

# as hash
sub hash {
    return {map {
        my $m = $_;
        exists $m->{match}[0]{''} ? () : (keys(%{$m->{match}[0]}))[0] => ref($_[0])->new($m)
    } @{${$_[0]}->{match}}};
}

# as array
sub array {
    return [map {
        my $m = $_;
        exists $m->{match}[0]{''} ? ref($_[0])->new($m) : ()
    } @{${$_[0]}->{match}}];
}

# ...

1;
