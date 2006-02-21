# pX/Common/iterator_engine_p6regex.pl - fglock
#
# experimental implementation of p6-regex parser
#
# see also: ../../Grammars/rx_grammar.pm

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
        # p5 code is called using: "rule { xyz { v5; ... } }" (audreyt on #perl6)
        # or: "rule { ... [:perl5:: this is p5 ] ... }"
        # or: "[:perl5(1) this is perl5 ]" (putter on #perl6)

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

# TODO - simplify this by using a ruleop::label - see implementation of 'star'
*rule::non_capturing_group =
do {
    my $r = ruleop::concat(
        ruleop::constant( '[' ),
        ruleop::concat(
            \&rule::rule,
            ruleop::constant( ']' )
        )
    );
    sub { 
        my ( $state, $match, $tail ) = $r->( @_ ); 
        return unless $match;
        $match = $match->[1];  # remove '['
        pop @$match;   # remove ']'
        #print Dumper( $match );
        ( $state, $match, $tail );
    }        
};

# TODO - simplify this by using a ruleop::label - see implementation of 'star'
*rule::capturing_group = 
do {
    my $r = ruleop::concat(
        ruleop::constant( '(' ),
        ruleop::concat(
            \&rule::rule,
            ruleop::constant( ')' )
        )
    );
    sub { 
        my ( $state, $match, $tail ) = $r->( @_ ); 
        return unless $match;
        $match = $match->[1];  # remove '('
        pop @$match;   # remove ')'
        #print Dumper( $match );
        ( $state, { capture => $match }, $tail );
    }        
};

sub ruleop::capture { 
    my $node = shift;
    sub {
        print "# *** ruleop::capture not implemented\n";
        $node->( @_ );
    }
}

*rule::term = ruleop::alternation(
        # \&rule::alternate,   # XXX fixme
        \&rule::ws,
        \&rule::closure,
        \&rule::subrule,
        \&rule::capturing_group,
        \&rule::non_capturing_group,
        # ruleop::constant( 'if' ),  # XXX - just an example
        \&rule::word,
        \&{'rule::.'},
      );

# XXX - doesn't work yet
*rule::alternate = 
    ruleop::concat(
        \&rule::rule,
        ruleop::optional (
            ruleop::concat(
                ruleop::constant( '|' ),
                \&rule::rule
            )
        )
    );

# rule 'rule' { [ <term>\* | <term> ]* }
# note: <term>\* creates a term named 'star'
*rule::rule = 
    ruleop::greedy_star( 
      ruleop::alternation( 
        ruleop::label( 'star', 
          ruleop::concat(
            \&rule::term,
            ruleop::constant( '*' ),
          ),
        ),
        \&rule::term,
      ),
    );

# XXX - Deep recursion on anonymous subroutine at iterator_engine.pl line 108.
# XXX - Deep recursion on anonymous subroutine at iterator_engine.pl line 83.
# *rule::rule = ruleop::greedy_star( \&rule::alternate );

#------ from http://svn.perl.org/parrot/trunk/compilers/pge/P6Rule.grammar

{
  # grammar PGE::P6Rule;
  package rule;

  # rule pattern { <flag>* <alternation> }
  *pattern = 
    ruleop::concat( 
      ruleop::greedy_star( 
        \&flag 
      ), 
      \&alternation 
    );

  # rule flag { \:<ident> [ \( <code> \) | \[ <code> \] ]? }
  *flag = 
    ruleop::concat( 
      ruleop::concat( 
        ruleop::constant( ':' ),
        \&ident
      ),
      ruleop::optional(
        ruleop::concat( 
          ruleop::concat( 
            ruleop::constant( '(' ),
            \&code
          ),
          ruleop::constant( ')' ),
        ),
        ruleop::concat( 
          ruleop::concat( 
            ruleop::constant( '[' ),
            \&code
          ),
          ruleop::constant( ']' ),
        ),
      )
    );

    # TODO

    sub code { print "<code> not implemented\n" };
    sub ident { print "<ident> not implemented\n" };
}

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
        #print "$tab $k => $v \n";
        if ( $k eq 'capture' ) {
            # return "$tab # XXX capture ?\n";
            return "$tab ruleop::capture(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'star' ) {
            pop @$v;  # remove '*'
            return "$tab ruleop::greedy_star(\n" .
                   emit_rule( $v, $tab ) . "$tab )\n";
        }        
        elsif ( $k eq 'code' ) {
            # return "$tab # XXX code - compile '$v' ?\n";
            return "$tab $v  # XXX - code\n";  
        }        
        elsif ( $k eq 'ws' ) {
            # ignore whitespace
            return;
            #return "$tab # <ws>\n";
        }
        elsif ( $k eq '.' ) {
            return "$tab \\&{'${namespace}.'}\n";
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

use Test::More qw(no_plan);
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Pad = '# ';
my $state;
my $tmp;
my ( $program, $compiled );
my ( $stat, $match, $tail );

{
  ( $stat, $match, $tail ) = rule::rule( '<word>' );
  ok ( defined $match, "parse rule" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  ( $stat, $match, $tail ) = $compiled->( '!some_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $match, $tail ) = rule::rule( '..' );
  ok ( defined $match, "parse rule - dot-dot" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  ( $stat, $match, $tail ) = $compiled->( '!' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $match, $tail ) = rule::rule( '<word> <ws>' );
  ok ( defined $match, "parse rule - 2 terms, with whitespace" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );
  is ( $tail, 'other', "remaining unmatched text (tail)" );

  ( $stat, $match, $tail ) = $compiled->( 'one_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $match, $tail ) = rule::rule( '<word> <ws>*' );
  ok ( defined $match, "parse rule - 2 terms, with star" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );
  is ( $tail, 'other', "remaining unmatched text (tail)" );

  # this test doesn't apply
  #( $stat, $match, $tail ) = $compiled->( 'one_word!' );
  #ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $match, $tail ) = rule::rule( '(<word>) <ws>' );
  ok ( defined $match, "parse rule - 2 terms, with capture" );
  $program = emit_rule( $match );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  # TODO - test captured text

  ( $stat, $match, $tail ) = $compiled->( 'one_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

{
  ( $stat, $match, $tail ) = rule::rule( '<word> [ <ws> <word> ]' );
  ok ( defined $match, "parse rule - non-capturing group" );
  $program = emit_rule( $match );
  #print $program;
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word other' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample of text" );

  ( $stat, $match, $tail ) = $compiled->( '!some_word' );
  ok ( !defined $match, "rejects unmatching text" );
}

__END__

XXX - deep recursion error

{
  ( $stat, $match, $tail ) = rule::rule( '<word>|<ws>' );
  ok ( defined $match, "parse rule - alternates" );
  $program = emit_rule( $match );
  ok ( defined $program, "emit rule to p5" );
  #print $program;
  $compiled = eval($program);
  is ( ref $compiled, "CODE", "compile p5" );
  ( $stat, $match, $tail ) = $compiled->( 'some_word' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 1" );
  ( $stat, $match, $tail ) = $compiled->( ' ' );
  # print Dumper( $match );
  ok ( defined $match, "parse sample 2" );
  ( $stat, $match, $tail ) = $compiled->( '!' );
  ok ( !defined $match, "rejects unmatching text" );
}

__END__

# TODO - test backtracking (implement '*' first)

# TODO - convert older tests to Test::More

print Dumper ruleop::rule()->( 0, '{ print 1+1, "\n" }' )
                     ->( 0, '' );

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
