# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser

use strict;
use warnings;

require 'iterator_engine.pl';

{
no strict "refs";
*{'rule::.'} = sub { 
        return unless $_[0];
        return ( undef, { '.'=> substr($_[0],0,1) }, substr($_[0],1) )
    };
}

sub rule::ws {
    return unless $_[0];
    return ( undef, { 'ws'=> $1 }, substr($_[0], 1) )
        if $_[0] =~ /^(\s)/;
    return;
};
sub rule::escaped_char {
    return unless $_[0];
    return ( undef, { 'escaped_char'=> $1 }, substr($_[0], 2) )
        if $_[0] =~ /^(\\.)/;
    return;
};
sub rule::word { 
    return unless $_[0];
    return ( undef, { 'word'=> $1 }, $2 )
        if $_[0] =~ /^([_[:alnum:]]+)(.*)/;
    return;
};

# rule compiler

sub rule::closure {
        my ( $code, $tail ) = $_[0] =~ /^\{(.*?)\}(.*)/;
        return unless defined $code;
        #print "parsing $code - $tail\n";
        my $result = eval $code;
        return ( undef, { code => $result }, $tail );
}

sub rule::subrule {
        my ( $code, $tail ) = $_[0] =~ /^\<(.*?)\>(.*)/;
        return unless defined $code;
        #print "parsing subrule $code\n";
        return ( undef, { rule => $code }, $tail );
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

#------ rule emitter

my $namespace = 'rule::';
sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1]; $tab .= '  ';
    local $Data::Dumper::Indent = 0;
    #print "emit_rule: ", ref($n)," ",Dumper( $n ), "\n";
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            push @s, emit_rule( $_, $tab );
        }
        return $s[0] unless $s[1];
        return $s[1] unless $s[0];

        return $s[0].$s[1] unless $s[0];
        return $s[0].$s[1] unless $s[1];

        #return $s[0].$s[1] if $s[0] =~ /^\s+#/;
        #return $s[0].$s[1] if $s[1] =~ /^\s+#/;

        return "$tab ruleop::concat(\n" . 
               $s[0] . "$tab ,\n" . $s[1] . "$tab )\n";

        return "$tab CONCAT (\n" . $s[0] . "$tab ," . $s[1] . "$tab )\n"
            if $s[1]; # =~ /^\s+\[/;
        return "$tab [\n" . $s[0].$s[1] . "$tab ]\n";
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        print "$tab $k => $v \n";
        if ( $k eq 'code' ) {
            # return "$tab # XXX code - compile '$v' ?\n";
            return "$tab $v\n";  
        }        
        elsif ( $k eq 'ws' ) {
            # ignore whitespace
            return;
            #return "$tab # <ws>\n";
        }
        elsif ( $k eq 'rule' ) {
            return "$tab \\&{'$namespace$v'}\n";
        }
        elsif ( $k eq 'constant' ) {
            return "$tab ruleop::constant( '$v' )\n";
        }
        elsif ( $k eq 'word' ) {
            return "$tab ruleop::constant( '$v' )\n";
        }
        else {
            die "unknown node: ", Dumper( $n );
        }
    }
    else 
    {
        die "unknown node: ", Dumper( $n );
    }
}

package main;

# TODO: use Test::More

use Data::Dumper;
$Data::Dumper::Indent = 1;

my $state;
my $tmp;

#print "compile rule\n";
my ( $stat, $match, $tail ) = 
    rule::rule( '<word> <ws> xyz' );

        # XXX - finish "parenthesis"
        # '<word> ( <word> ) xyz' );

        # XXX - finish "closure"
        # '{ "<wo"."rd>" } <word> ( <word> ) xyz' );
#print "run rule \n", Dumper( $stat, $match, $tail );
#print "run rule \n", 
print Dumper( $match );
#$match->( 0, '' );

my $s = emit_rule( $match );
print "-- Start program\n$s\n-- End program\n";

my $compiled_rule = eval($s);

{
my $test_string = 'some_word xyz';
print "parsing '$test_string'\n";
my ( $stat, $match, $tail ) = 
    $compiled_rule->( 'some_word xyz' );
print Dumper( $match );
}

__END__

print Dumper ruleop::rule()->( 0, '{ print 1+1, "\n" }' )
                     ->( 0, '' );

__END__

print Dumper ruleop::rule( 0, ' ' )
                     ->( 0, 'x' );

print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, ' abc def' );

print Dumper ruleop::rule( 0, 'abc' )
                     ->( 0, 'abc' );
print Dumper ruleop::rule( 0, '<word>' )
                     ->( 0, 'abc' );

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
