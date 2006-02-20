# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser

use strict;
use warnings;

require 'iterator_engine.pl';

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
*rule::word = ruleop::concat(
        \&rule::word_char,
        ruleop::greedy_star( \&rule::word_char )
    );

# rule compiler

sub rule::closure {
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

sub rule::subrule {
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
}

*rule::parenthesis = 
    ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::concat(
            \&rule::rule,
            ruleop::constant( ')' )
        )
    );

*rule::rule = 
    ruleop::greedy_star(
      ruleop::alternation(
        \&rule::ws,
        \&rule::closure,
        \&rule::subrule,
        \&rule::parenthesis,
        # ruleop::constant( 'if' ),  # XXX - just an example
        \&rule::word,
      )
    );

package main;

# TODO: use Test::More

use Data::Dumper;
$Data::Dumper::Indent = 1;

my $state;
my $tmp;

#print "compile rule\n";
my ( $stat, $match, $tail ) = 
    rule::rule( 0, split //, 
        '{ 1+1 } <word> ( <word> ) xyz' );
#print "run rule \n", Dumper( $stat, $match, $tail );
#print "run rule \n", 
print Dumper( $match );
#$match->( 0, split //, '' );

__END__

print Dumper ruleop::rule()->( 0, split //, '{ print 1+1, "\n" }' )
                     ->( 0, split //, '' );

__END__

print Dumper ruleop::rule( 0, split //, ' ' )
                     ->( 0, split //, 'x' );

print Dumper ruleop::rule( 0, split //, '<word>' )
                     ->( 0, split //, ' abc def' );

print Dumper ruleop::rule( 0, split //, 'abc' )
                     ->( 0, split //, 'abc' );
print Dumper ruleop::rule( 0, split //, '<word>' )
                     ->( 0, split //, 'abc' );

__END__

=for later

$state = 0;
{
    while ( defined $state ) {
        ($state, $tmp, undef) = 
            ruleop::greedy_plus( rule::constant( 'ab' ) )
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
