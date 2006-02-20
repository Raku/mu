# pX/Common/iterator_engine.pl - fglock
#
# status: the implementation uses fast ARRAY operations, 
# but this makes it difficult to write regex compositions
# such as alternations, and so it doesn't scale easily to complex regexes
#
# plan: rewrite using generators instead of ARRAY
# problem: this may be too slow, or difficult to maintain

use strict;
use warnings;

=pod

A "rule" function gets as argument:

- a "continuation" (or a zero, to get the first match)
- a "string" (a list of characters) to match

it returns (or "yields"):

- a "continuation" or undef
- a "match" or undef
- the string tail (a list of characters) or undef

Continuations are used for backtracking.

=cut

# internal functions that perform rule composition

# XXX - optimization - pass the string index around, 
# XXX   instead of copying the whole string to @tail every time

sub rule::greedy_plus { 
    my $node = shift;
    my $alt;
    $alt = rule::concat( 
        $node, 
        rule::optional( sub{$alt->(@_)} )
    );
    return $alt;
}
sub rule::greedy_star { 
    my $node = shift;
    return rule::optional( rule::greedy_plus( $node ) );
}

sub rule::non_greedy { 
    my $node = shift;
    # TODO
    # XXX - possible implementation strategy - reuse rule::concat ?
    return sub {
        my $n = shift;
        my @tail = @_;
        my @matches;
        my @cont;
        for (1 .. ++$n) {
                my ($state, $match, @new_tail) = $node->(0, @tail);
                return unless $match;
                @tail = @new_tail;
                push @matches, $match;
                push @cont, $state;
        }
        # XXX - return ( @continuation, ...
        # TODO - post-process @cont for proper permutation
        return ( \@cont, { 'non_greedy' => [ @matches ] }, @tail );
    }
}

sub rule::alternation {
    # XXX - is this supposed to return the longest match first?
    # XXX   in which case it would have to test all possibilities before returning
    my @nodes = @_;
    return sub {
        my $n = shift;
        #print "testing alternations on @_\n";
        return unless @nodes;
        my $match;
        my @tail;
        $n = [ 0, 0 ] if $n == 0;
        my $state = [ $n->[0], $n->[1] ];
        while( defined $state ) {
            ($state->[1], $match, @tail) = $nodes[ $state->[0] ]->( $state->[1], @_);
            if ( ! defined $state->[1] ) {
                $state->[0]++;
                $state->[1] = 0;
                $state = undef if $state->[0] > $#nodes;
            }
            return ( $state, { 'alternation' =>$match }, @tail) if $match;
        }
        return;
    }
}

sub rule::concat {
    my @concat = @_;
    # TODO: generalize for @concat > 2
    return sub {
        my $n = shift;
        my @matches;
        my @tail;
        my $state;
        my $state0 = ref($n) ? $n->[0] : 0;
        my $state1 = ref($n) ? $n->[1] : 0;
        while (1) {
            ($state, $matches[0], @tail) = $concat[0]->($state0, @_);
            #print "  1st match: ", Dumper( [ $state, $matches[0], @tail ] );
            if ( ! defined $matches[0] ) {
                return unless defined $state;
                $state0 = $state;
                $state1 = 0;
                next;
            }
            ($state1, $matches[1], @tail) = $concat[1]->($state1, @tail);
            #print "  2nd match: ", Dumper( [ $state, $matches[1], @tail ] );
            if ( defined $matches[1] ) {
                my $succ;
                if ( ! defined( $state1 ) ) {
                    $succ = [ $state, 0 ] if defined $state;
                }
                else {
                    $succ = [ $state0, $state1 ];
                }
                return ( $succ, { 'concat'=>[ @matches ] }, @tail);
            }
            if ( !defined( $state1 ) ) {
                return unless defined $state;
                $state0 = $state;
                $state1 = 0;
            }
            #print "backtracking: $state1 - $state0\n";
        }
    }
}

sub rule::constant { 
    my $const = shift;
    my @const = split //, $const;
    return sub {
        return if +shift;  # no continuation
        return if $#const > $#_;
        for ( 0 .. $#const ) {
            return if $const[$_] ne $_[$_];
        }
        return ( undef, $const, @_[@const..$#_] );
        return;
    }
}
sub rule::optional {
    return rule::alternation( $_[0], \&rule::null );
}

# Prelude - precompiled rules, such as <word>, \x, etc.

sub rule::null {
    return if +shift;  # no continuation
    ( undef, [], @_ );
};

{
no strict "refs";
*{'rule::.'} = sub { 
        return if +shift;  # no continuation
        @_ ? ( undef, { '.'=> $_[0] }, @_[1..$#_] ) : undef
    };
}

sub rule::ws {
    return if +shift;  # no continuation
    return unless @_;
    return ( undef, { 'ws' => $_[0] }, @_[1..$#_] )
        if $_[0] =~ /\s/;
    return;
};
sub rule::slashed_char {
    return if +shift;  # no continuation
    return if @_ < 2;
    return ( undef, { 'slashed_char' => [ $_[0], $_[1] ] }, @_[2..$#_] ) 
        if $_[0] eq '\\';
    return;
};
sub rule::word_char { 
    return if +shift;  # no continuation
    return unless @_;
    return ( undef, { 'word_char'=> $_[0] }, @_[1..$#_] ) 
        if $_[0] =~ m/[a-zA-Z0-9\_]/;  
    return;
};
*rule::word = rule::concat(
        \&rule::word_char,
        rule::greedy_star( \&rule::word_char )
    );

# rule compiler

sub rule::closure {
    return sub {
        return if +shift;
        return if !@_;
        return if $_[0] ne '{';
        shift;
        my $code;
        while ( @_ ) {
            last if $_[0] eq '}';
            $code .= +shift;
        }
        return if $_[0] ne '}';
        shift;
        #print "compiling $code - @_\n";
        my $result = eval $code;
        return ( undef, { code => $result }, @_ );
    }
}

sub rule::subrule {
    return sub {
        return if +shift;
        return if !@_;
        return if $_[0] ne '<';
        shift;
        my $code;
        while ( @_ ) {
            last if $_[0] eq '>';
            $code .= +shift;
        }
        return if $_[0] ne '>';
        shift;
        #print "subrule $code\n";
        return ( undef, { rule => $code }, @_ );
        #{
        #    no strict "refs";
        #    return &{ 'rule::' . $code }( @_ );
        #}
    }
}

sub rule::rule {
    rule::greedy_star(
      rule::alternation(
        \&rule::ws,
        rule::closure,
        rule::subrule,
        \&rule::word,
      )
    );
}

package main;

# TODO: use Test::More

use Data::Dumper;
$Data::Dumper::Indent = 1;

my $state;
my $tmp;

print "compile rule\n";
my ( $stat, $match, $tail ) = 
    rule::rule()->( 0, split //, '{ print 1+1, "\n"; 3 } <word>' );
print "run rule \n", Dumper( $stat, $match, $tail );
#$match->( 0, split //, '' );

__END__

print Dumper rule::rule()->( 0, split //, '{ print 1+1, "\n" }' )
                     ->( 0, split //, '' );

__END__

print Dumper rule::rule( 0, split //, ' ' )
                     ->( 0, split //, 'x' );

print Dumper rule::rule( 0, split //, '<word>' )
                     ->( 0, split //, ' abc def' );

print Dumper rule::rule( 0, split //, 'abc' )
                     ->( 0, split //, 'abc' );
print Dumper rule::rule( 0, split //, '<word>' )
                     ->( 0, split //, 'abc' );

__END__

=for later

$state = 0;
{
    while ( defined $state ) {
        ($state, $tmp, undef) = 
            rule::greedy_plus( rule::constant( 'ab' ) )
            ->( $state, qw(a b a b a b) );
        print "recursive\n", Dumper( $tmp );
    }
}

__END__

=for tested

$state = 0;
{
    my $alt = rule::alternation( 
        rule::constant('ab'), rule::constant('ba'), rule::constant('a') );
    while ( defined $state ) {
        ($state, $tmp, undef) = rule::concat( 
                $alt, rule::optional( $alt ) 
            )->( $state, qw(a b a)
        );
        print "concat\n", Dumper( $tmp );
    }
}

$state = 0;
while ( defined $state ) {
    print "alternation\n", Dumper( 
        ($state, $tmp, undef) = rule::alternation( 
            rule::constant('ab'), rule::constant('a') 
        )->( $state, qw(a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( rule::constant('a') )->( $state, qw(a a b c) ) 
    );
}

$state = 0;
while ( defined $state ) {
    print "greedy + alternation state\n", Dumper( 
        ($state, $tmp, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('ab'), rule::constant('a'), 
                rule::constant('cd'), rule::constant('ba'), 
                rule::constant('bc'), )
        )->( $state, qw(a b a b c) ) 
    );
}

=cut

__END__

$state = 0;
for ( 0 .. 3 ) {
    print "greedy + alternation state $_\n", Dumper( 
        ($state, undef, undef) = rule::greedy( 
            rule::alternation( 
                rule::constant('cd'), rule::constant('ab'), 
                rule::constant('a'),  rule::constant('cd') )
        )->( $state, qw(a b a b c) ) 
    );
}

__END__

print "any-char\n", Dumper( 
  &{'rule::.'}( 0, qw( a b ) ) 
);

print "greedy\n", Dumper( 
  rule::greedy( rule::constant('a') )->( 0, qw(a a a b c) ) 
);
print "greedy backtrack\n", Dumper( 
  rule::concat( 
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'} 
  )->( 0, qw(a a a a) ) 
);
print "greedy no-match\n", Dumper( 
  rule::concat(
    rule::greedy( rule::constant('a') ),
    \&{'rule::.'}
  )->( 0, qw(b a a a a) ) 
);
print "word\n", Dumper( 
  &{'rule::<word>'}( 0, qw(b a a ! !) ) 
);
print "word concat\n", Dumper( 
  rule::concat( \&{'rule::<word>'}, \&{'rule::<ws>'} )->( 0, qw(b a ),' ' ) 
);
print "non_greedy + backtracking\n", Dumper( 
  rule::concat(
    rule::non_greedy( rule::constant('a') ),
    rule::constant('ab')
  )->( 0, qw(a a a a b) ) 
);
print "alternation + backtracking\n", Dumper( 
  rule::concat(
    rule::alternation( rule::constant('a'), rule::constant('ab') ),
    rule::constant('ab')
  )->( 0, qw(a b a b) ) 
);
print "alternation + greedy + backtracking -- (ab,a,ab)(ab)\n", Dumper( 
  rule::concat(
    rule::greedy(
      rule::alternation( rule::constant('a'), rule::constant('ab') )
    ),
    rule::constant('ab')
  )->( 0, qw(a b a a b a b) ) 
);
