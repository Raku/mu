package Pugs::Runtime::RuleInline;

# - fglock
#
use strict;
use warnings;
use Data::Dumper;
use PadWalker qw( peek_my );  # peek_our ); ???

sub alternation {
    return '( ' . join( ' || ', @_ ) . ' )';
}

sub concat {    
    return '( ' . join( ' && ', @_ ) . ' )';
}

sub constant { 
    my $const = shift;
    return "( ( \$s =~ m/^(\Q$const\E)(.*)/s ) ? ( \$s = \$2, push \@match, \$1 ) : 0 )";
}

sub null {
    "1";
};

sub wrap {
    return eval( "sub { my \@match; my \$s = shift; $_[0]; return \$s, \\\@match; }" );
}

my $r = 
    concat( 
        alternation( constant('a'), constant('b') ),
        constant('b'),
    );
print $r;
my $x = wrap( $r );
print Dumper( $x->("abc") );

use Benchmark;
use Pugs::Compiler::Rule;
my $rpcr = Pugs::Compiler::Rule->compile('[a|b]b');
Benchmark::cmpthese(1000, {
    PCR => sub{$rpcr->match('abc')},
    fast_x1000 => sub{$x->('abc') for 1..1000},
});

__END__

sub capture {
    # return a labeled capture
    my $label = shift;
    my $node = shift;
    sub {
        $_[3] = { label => $label };
        my $match = $node->( @_[0,1,2], $_[3]{match} );
        $match = $$match if ref($match) eq 'Pugs::Runtime::Match';
        return unless $match->{bool};
        ## return if $match->{abort}; - maybe a { return }
        my $new_match = { %$match };
        
        $new_match->{label}   = $label;
    
        if ( ! defined $new_match->{capture} ) {
            if ( defined $match->{tail} ) {
                my $len = length( $match->{tail} );
                my $head = $len?substr($_[0], 0, -$len):$_[0];
                $new_match->{capture} = $head;   # XXX -- array ref not needed
                #print 'got capture: ',do{use Data::Dumper; Dumper($new_match)};
            }
            else {
                $new_match->{capture} = $_[0];
            }
        }
        $new_match->{match}   = $match ;  # XXX - workaround

        # print "Capturing ", Dumper($_[2]);

        return $_[3] = $new_match;
    }
}

# experimental!
sub try { 
    my $op = shift;
    return sub {
        my $match = $op->( @_ );
        ### abortable match...
        $match->{abort} = 0;
        return $match;
    };
};

# experimental!
sub abort { 
    my $op = shift;
    return sub {
        my $match = $op->( @_ );
        ### aborting match: $match
        $match->{abort} = 1;
        return $match;
    };
};

# experimental!
sub negate { 
    my $op = shift;
    return sub {
        #my $tail = $_[0];
        my $match = $op->( @_ );
        return if $match->{bool};
        return { bool => 1,
                 #match => undef,  #'null',
                 tail => $_[0],
               }
    };
};

# experimental!
=for example
    # adds an 'before' or 'after' sub call, which may print a debug message 
    wrap( { 
            before => sub { print "matching variable: $_[0]\n" },
            after  => sub { $_[0]->{bool} ? print "matched\n" : print "no match\n" },
        },
        \&variable
    )
=cut
sub wrap {
    my $debug = shift;
    my $node = shift;
    sub {
        $debug->{before}( @_ ) if $debug->{before};
        my $match = $node->( @_ );
        $debug->{after}( $match, @_ ) if $debug->{after};
        return $match;
    }
}

sub perl5 {
    my $rx = qr(^($_[0])(.*)$)s;
    #print "rx: $rx\n";
    return sub {
        return unless defined $_[0];
        if ( $_[0] =~ m/$rx/ ) {
            return $_[3] = { 
                bool  => 1,
                match => $1,
                tail  => $2,
                capture => $1,
            };
        }
        return;
    };
}


# ------- higher-order ruleops

sub optional {
    return alternation( [ $_[0], null() ] );
}

sub null_or_optional {
    return alternation( [ null(), $_[0] ] );
}

sub greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = concat( 
        $node, 
        optional( sub{ goto $alt } ),  
    );
    return $alt;
}

sub greedy_star { 
    my $node = shift;
    return optional( greedy_plus( $node ) );
}

sub non_greedy_star { 
    my $node = shift;
    alternation( [ 
        null(),
        non_greedy_plus( $node ) 
    ] );
}

sub non_greedy_plus { 
    my $node = shift;

    # XXX - needs optimization for faster backtracking, less stack usage

    return sub {
        my $tail =  $_[0];
        my $state = $_[1] || { state => undef, op => $node };
        #my $flags = $_[2];

        # XXX - didn't work
        # my $match = $state->{op}->( $tail, $state->{state}, $flags ); 

        my $match = $state->{op}->( $tail, undef );
        return unless $match->{bool};
        $match->{state} = {
            state => $match->{state},
            op    => concat( $node, $state->{op} ),
        };
        return $match;
    }
}

# interface to the internal rule functions
# - creates a 'capture', unless it detects a 'return block'
sub rule_wrapper {
    my ( $str, $match ) = @_;
    $match = $$match if ref($match) eq 'Pugs::Runtime::Match';
    return unless $match->{bool};
    if ( $match->{return} ) {
        #warn 'pre-return: ', Dumper( $match );
        my %match2 = %$match;
        $match2{capture} = $match->{return}( 
            Pugs::Runtime::Match->new( $match ) 
        );
        #warn "return ",ref($match2{capture});
        #warn 'post-return: ', Dumper( $match2{capture} );
        delete $match->{return};
        delete $match->{abort};
        delete $match2{return};
        delete $match2{abort};
        #warn "Return Object: ", Dumper( \%match2 );
        return \%match2;
    }
    #warn "Return String";
    # print Dumper( $match );
    my $len = length( $match->{tail} );
    my $head = $len ? substr($str, 0, -$len) : $str;
    $match->{capture} = $head;
    delete $match->{abort};
    return $match;
}

# not a 'rule node'
# gets a variable from the user's pad
# this is used by the <$var> rule
sub get_variable {
    my $name = shift;
    
    local $@;
    my($idx, $pad) = 0;
    while(eval { $pad = peek_my($idx) }) {
        $idx++, next
          unless exists $pad->{$name};

        return ${ $pad->{$name} };
    }
    die "Couldn't find '$name' in surrounding lexical scope.";
}

1;
