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

# 2 versions - match anywhere; match at $pos
sub constant_at_pos { 
    my $const = shift;
    my $len = length( $const );
    '( ( substr( $s, $pos, '.$len.' ) eq \''.$const.'\' ) 
    ? do { $pos += '.$len.'; push @match, \''.$const.'\' }
    : 0
)'
}
sub constant_anywhere { 
    my $const = shift;
    "( ( \$s =~ m/\Q$const\E/s ) 
    ? do { \$pos = pos \$s; push \@match, $const } 
    : 0 
)"
}

# implements <after>
# $pos must be saved
sub constant_reversed { 
    my $const = shift;
    my $len = length( $const );
    '( ( substr( $s, $pos - '.$len.', '.$len.' ) eq \''.$const.'\' ) 
    ? do { $pos -= '.$len.' }
    : 0
)'

sub greedy_star {
    "do { while @_ {} }"
}

sub null {
    "1";
};

sub wrap {
    return "sub { 
    my \@match; my \$pos = 0; my \$s = shift; 
    $_[0]; 
    return \\\@match; \n}\n";
}

my $r = 
    concat( 
        alternation( constant_at_pos('a'), constant_at_pos('b') ),
        constant_at_pos('b'),
        greedy_star( constant_at_pos('x') ),
    );
print wrap( $r );
my $x = eval wrap( $r );
print Dumper( $x->("abxxxd") );

use Benchmark;
use Pugs::Compiler::Rule;
my $rpcr = Pugs::Compiler::Rule->compile('[a|b]bx*');
Benchmark::cmpthese(1000, {
    PCR_x10 => sub{$rpcr->match('abxxxd') for 1..10},
    fast_x100 => sub{$x->('abxxxd') for 1..100},
});

__END__


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
