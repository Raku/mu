
# see pod in the end

package Match;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

sub new {
    my ($class, $match) = @_;
    my $self = \$match;
    bless $self, $class;
}

sub _box_submatch {
    my ($match, $submatch) = @_;
    $m = { %$submatch };
    delete $m->{label};
    ref($match)->new($m);
}

sub flat {
    ${$_[0]}->{capture};
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
        exists $m->{label} 
        ? ( $m->{label} eq '' 
            ? () 
            : $m->{label} => _box_submatch( $_[0], $m )
          )
        : ()
    } @{${$_[0]}->{match}}};
}

# as array
sub array {
    #print 'array from: ', do{use Data::Dumper; Dumper($_[0])};
    # special case: the whole match is a single capture
    if ( exists ${$_[0]}->{label} && ${$_[0]}->{label} eq '' ) {
        my $m = _box_submatch( $_[0], ${$_[0]} );
        return [ $m ];
    }
    return [map {
        my $m = $_;
        #print 'array element: ', do{use Data::Dumper; Dumper($m)};
        exists $m->{label} && $m->{label} eq '' 
        ? _box_submatch( $_[0], $m )
        : ()
    } @{${$_[0]}->{match}}];
}

# ...

1;

__END__

=pod

Match

=cut

