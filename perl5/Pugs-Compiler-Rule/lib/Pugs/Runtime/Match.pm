
package ::Match;

sub new {
    bless $_[1], $_[0];  
}

# return the capture
sub capture {
    $_[0]->{capture};
}

# return the bool value
sub bool {
    $_[0]->{bool};
}

# as string

# as hash

# as array
sub array {
    @{ $_[0]->{match} };
}

# ...

1;
