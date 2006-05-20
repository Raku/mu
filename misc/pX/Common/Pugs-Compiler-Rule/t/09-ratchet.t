
use Test::More tests => 42;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

use Pugs::Runtime::Match::Ratchet; # overload doesn't work without this ???

{
    package test;
    use base Pugs::Grammar::Base;
}

{
    package test2;
    use base Pugs::Grammar::Base;
}

{
    # unnamed rules are objects
    my $rule = Pugs::Compiler::Rule->compile( '((.).)(.)', { ratchet => 1 } );
    my $match = $rule->match( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
    is( "$match->[1]", "z", 'stringify 4' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( 'ab|c', { ratchet => 1, p => 0 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( !$match, "basic alternative" );
    my $match = $rule->match("ab");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "basic alternative - 2" );
}

{
    my $rule = Pugs::Compiler::Rule->compile( 'ab|ac', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "alternation no backtracking" );
}

{
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Rule->compile( '((.).)(.)', { ratchet => 1 } )->code;
    #print "Source: ", do{use Data::Dumper; Dumper(Pugs::Compiler::Rule->compile( '((.).)(.)', { ratchet => 1 } )->{perl5})};
    my $match = test->rule_method( "xyzw" );
    is( "$match", "xyz", 'named rules are methods' );
}

{
    # calling named subrules
    *test::rule_method3 = Pugs::Compiler::Rule->compile( '.', { ratchet => 1 } )->code;
    *test::rule_method4 = Pugs::Compiler::Rule->compile( '<rule_method3>', { ratchet => 1 } )->code;
    #print "Source: ", do{use Data::Dumper; Dumper(Pugs::Compiler::Rule->compile( '<rule_method3>', { ratchet => 1 } )->{perl5})};
    my $match = test->rule_method4( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in same grammar' );
}

{
    # calling named subrules in other grammars
    *test2::rule_method = Pugs::Compiler::Rule->compile( '.', { ratchet => 1 } )->code;
    *test::rule_method5 = Pugs::Compiler::Rule->compile( '<test2.rule_method>', { ratchet => 1 } )->code;
    my $match = test->rule_method5( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in other grammar' );
}

{
    # calling unnamed subrules
    $test2::rule2 = Pugs::Compiler::Rule->compile( '.' );
    *test::rule_method2 = Pugs::Compiler::Rule->compile( '<$test2::rule2>', { ratchet => 1 } )->code;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a global unnamed subrule' );
}

{
    # calling unnamed subrules
    my $match;
    my $rule2 = Pugs::Compiler::Rule->compile( '.', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule2->perl5 )};
    eval {
    *test::rule_method6 = Pugs::Compiler::Rule->compile( '<$rule2>', { ratchet => 1 } )->code;
    $match = test->rule_method6( "xyzw" );
    };
    warn "# *** Please check if CPAN module 'PadWalker' is properly installed\n",
         "# *** This is the resulting error: $@"
        if $@;
    #print "Source: ", do{use Data::Dumper; Dumper( Pugs::Compiler::Rule->compile( '<$rule2>', { ratchet => 1 } )->perl5 )};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a lexical unnamed subrule' );
}

{
    # generated rules
    my $rule = Pugs::Compiler::Rule->compile( '<alpha>+', { ratchet => 1 } );
    my $match = $rule->match( "xy12" );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 ) };
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xy", 'built-in rule <alpha>' );
}

{
    # not-special chars
    my $rule = Pugs::Compiler::Rule->compile( ',', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 )};
    my $match = $rule->match( "," );
    is( "$match", ",", 'comma is not a special char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\(', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    my $match = $rule->match( "(xy12)" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "(", 'escaped char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\n', { ratchet => 1 } );
    my $match = $rule->match( "\nxy12" );
    is( "$match", "\n", 'escaped char \\n' );
}

#TODO: 
{
    #local $TODO = "usage of :!p in token is not specified";
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\d', { ratchet => 1 } );
    my $match = $rule->match( "abc123" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "1", 'escaped char \\d' );
}

#TODO: 
{
    #local $TODO = "usage of :!p in token is not specified";
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\D', { ratchet => 1 } );
    my $match = $rule->match( "123abc" );
    is( "$match", "a", 'escaped char \\D' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\d', { ratchet => 1 } );
    my $match = $rule->match( "123" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "1", 'escaped char \\d' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\D', { ratchet => 1 } );
    my $match = $rule->match( "abc" );
    is( "$match", "a", 'escaped char \\D' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\N', { ratchet => 1 } );
    my $match = $rule->match( "\n\n" );
    is( "$match", "", 'escaped char \\N' );
    $match = $rule->match( "xy12" );
    is( "$match", "x", 'escaped char \\N #2' );
}

{
    #local $TODO = "quantifiers not implemented yet";
    # ambiguous rule /a?bg?/
    # XXX - is this /a? [bg]?/ or /a? b g?/
    # --- It should the same as /a? b g?/
    # 1) spaces should not make difference
    # 2) the other way, it should be as /[a?[bg]]?/
    my $rule = Pugs::Compiler::Rule->compile( 'a?bg?', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    
    my $match = $rule->match("bprw");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is("$match","b",'"a?bg?" equals "a? b g?".');
   
    # this string will not match, because /a?/ matches before c, and the /b/ fails
    #my $match = $rule->match("cdtbprw");
    #is("$match","b",'"a?bg?" equals "a? b g?".');
}

{
    # capture
    my $rule = Pugs::Compiler::Rule->compile('some (text) { return { a => $_[0][0]() ,} } ', { ratchet => 1 });
    #my $rule = Pugs::Compiler::Rule->compile('some (text) { return { a => $_[0][0] ,} } ', { ratchet => 1 });
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    my $match = $rule->match("sometext");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    my $capture = $match->();
    is(ref($capture),'HASH','Capture is a hashref');
    is($capture->{a},'text','$capture->{a}');
}

{
    # XXX - is $() working?
    # capture
    my $rule = Pugs::Compiler::Rule->compile('some (text) { return { a => $() ,} } ', { ratchet => 1 });
    my $match = $rule->match("sometext");
    #print Dumper($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}

{
    # alternation
    my $rule = Pugs::Compiler::Rule->compile('[a|b](b)', { ratchet => 1 } );
    my $match = $rule->match( "bb" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "bb", 'alternation' );
}

{
    # basic named capture
    my $rule = Pugs::Compiler::Rule->compile('$<cap> := (<ws>)', { ratchet => 1 } );
    my $match = $rule->match( " " );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", " ", 'named capture' );
    is( $match->{'cap'}, " ", 'named capture - 2' );
}

#TODO:
{
    #local $TODO = ":p broken in non-ratchet subrule call";
    
    # basic named capture
    my $rule = Pugs::Compiler::Rule->compile('a<ws>', { ratchet => 1 } );
    my $match = $rule->match( "a b" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a ", 'named capture from subrule' );
    is( $match->{'ws'}, " ", 'named capture - 2' );
}

{
    # before
    my $rule = Pugs::Compiler::Rule->compile('a<before b>', { ratchet => 1 } );
    my $match = $rule->match( "ab" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a", 'before' );

    $match = $rule->match( "ac" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'not before' );

    # TODO: <!before b>
}

{
    # $
    my $rule = Pugs::Compiler::Rule->compile('a$', { ratchet => 1 } );
    my $match = $rule->match( "ab" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'not end-of-str' );

    $match = $rule->match( "a" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a", 'end-of-str' );
}

{
    # .
    my $rule = Pugs::Compiler::Rule->compile('a..', { ratchet => 1 } );
    my $match = $rule->match( "ab" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'dot underflow' );

    $match = $rule->match( "abc" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "abc", 'dot' );

    $match = $rule->match( "abcd" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "abc", 'dot overflow' );
}

{
    # after
    my $rule1 = Pugs::Compiler::Rule->compile('<after xyz>a', { ratchet => 1 } );
    my $rule = Pugs::Compiler::Rule->compile('...<$rule1>', { ratchet => 1 } );
    my $match = $rule->match( "xyzab" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Source: ", do{use Data::Dumper; Dumper($rule1->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xyza", 'after' );

    $match = $rule->match( "xyyac" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'not after' );

    # TODO: <!after b>
}
