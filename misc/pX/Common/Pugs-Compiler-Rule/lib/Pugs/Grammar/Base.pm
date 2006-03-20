package Pugs::Grammar::Base;
use Pugs::Runtime::Match;

# defines <ws>, unicode character classes, etc

my $no_match;
BEGIN {
    $no_match = Pugs::Runtime::Match->new( { bool => 0 } );
}

sub ws {
    my $class = shift;
    return $no_match unless $_[0];
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
        capture => $1,
    } )
        if $_[0] =~ /^(\s+)(.*)$/s;
    return $no_match;
};

# not in spec
sub any {
    my $class = shift;
    return $no_match unless $_[0];
    return Pugs::Runtime::Match->new( { 
        bool  => 1,
        match => $1,
        tail  => $2,
    } )
        if $_[0] =~ /^(.)(.*)$/s;
    return $no_match;
};

BEGIN {
    # this list was extracted from 'perlre'
    for my $char_class ( qw( 
alpha
alnum
ascii
blank
cntrl
digit
graph
lower
print
punct
space
upper
word
xdigit
) ) {
        my $rx = qr(^([[:$char_class:]])(.*)$);
        *{$char_class} = sub {
            my $class = shift;
            return $no_match unless $_[0];
            #my ($test, $tail) = $_[0] =~ /$rx/;
            #warn "Matching $char_class in [$_[0]] == [$test,$tail]";
            return Pugs::Runtime::Match->new( { 
                bool  => 1,
                match => $1,
                tail  => $2,
                capture => $1,
            } )
                if $_[0] =~ /$rx/;
            return $no_match;
        };
    }
}

1;
