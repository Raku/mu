
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

sub new {
    my ($class, $match) = @_;
    my $self = \$match;
    bless $self, $class;
}

sub flat {
    # match::str(${$_[0]});
    join '', map { values %{$_->{match}} } @{${$_[0]}->{match}};
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
        exists $_->{''}
            ? ()
            : map { ref $_ ? @$_ : $_ } %$_
    } @{${$_[0]}->{capture}}};
}

# as array
sub array {
    return [map {
        exists $_->{''}
            ? @{$_->{''}}
            : ()
    } @{${$_[0]}->{capture}}];
}

# ...

1;
