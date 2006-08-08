package Pugs::Grammar::LrepBase;
use Pugs::Runtime::LrepMatch;

# This class defines <ws>, unicode character classes, etc
# runtime parameters are: $grammar, $string, $flags, $state

# internal methods - not in spec

sub no_match { 
    Pugs::Runtime::LrepMatch->new( { bool => 0 } );
}
             
sub any {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my $pos = $_[1]{p} || 0;
    return Pugs::Runtime::LrepMatch->new( { 
        bool  => 1,
        str   => $_[0],
        match => $1,
        tail  => $2,
        capture => $1,
        from  => $pos,
        to    => $pos+1,
    } )
        if $_[0] =~ /^.{$pos}(.)(.*)$/s;
    return $grammar->no_match;
};

# specced methods

sub ws {
    my $grammar = shift;
    return $grammar->no_match unless $_[0];
    my $pos = $_[1]{p} || 0;
    #print "POS $pos ";
    return Pugs::Runtime::LrepMatch->new( { 
        bool  => 1,
        str   => $_[0],
        match => $1,
        tail  => $2,
        capture => $1,
        from  => $pos,
        to    => $pos+length $1,
    } )
        if $_[0] =~ /^.{$pos}((?:(?<!\w)|(?!\w)|\s)\s*)/s;
        #if $_[0] =~ /^.{$pos}(\s+)(.*)$/s;
    return $grammar->no_match;
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
        #my $rx = qr(^.{$pos}([[:$char_class:]])(.*)$);
        *{$char_class} = sub {
            my $grammar = shift;
            return $grammar->no_match unless $_[0];
            my $pos = $_[1]{p} || 0;
            #my ($test, $tail) = $_[0] =~ /$rx/;
            #warn "Matching $char_class in [$_[0]] == [$test,$tail]";
            return Pugs::Runtime::LrepMatch->new( { 
                bool  => 1,
                str   => $_[0],
                match => $1,
                tail  => $2,
                capture => $1,
                from  => $pos,
                to    => $pos+length $1,
            } )
                if $_[0] =~ # /$rx/;
                    /^.{$pos}([[:$char_class:]])(.*)$/s;
            return $grammar->no_match;
        };
    }
}

1;
