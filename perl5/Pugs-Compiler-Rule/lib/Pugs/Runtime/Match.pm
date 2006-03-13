
package Match;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '""'     => \&flat, # fixed (fglock) XXX - wrong - should use "result object"
    '0+'     => \&flat, # fixed (fglock) XXX - wrong - should use "result object"
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
    ${$_[0]}->{capture};
    # _str(${$_[0]});
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
        exists $m->{label} && $m->{label} eq '' 
        ? () 
        : (keys(%{$m->{match}[0]}))[0] => ref($_[0])->new($m)
    } @{${$_[0]}->{match}}};
}

# as array
sub array {
    #print 'array from: ', do{use Data::Dumper; Dumper($_[0])};
    # special case: the whole match is a single capture
    if ( exists ${$_[0]}->{label} && ${$_[0]}->{label} eq '' ) {
        my $m = { %${$_[0]} };
        delete $m->{label};
        #print "array element: Returning first match: \n", do{use Data::Dumper; Dumper($m)};
        #print 'array element: Returning original match: ', do{use Data::Dumper; Dumper($_[0])};
        my @a = ( ref($_[0])->new($m) );
        return \@a;
    }
    return [map {
        my $m = $_;
        #print 'array element: ', do{use Data::Dumper; Dumper($m)};
        exists $m->{label} && $m->{label} eq '' 
        ? do{
            $m = { %$m };
            delete $m->{label};
            ref($_[0])->new($m);
        }
        : ()
    } @{${$_[0]}->{match}}];
}

# ...

1;
