
use Test::More tests => 78;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Token' );
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
    my $rule = Pugs::Compiler::Token->compile( '((.).)(.)' );
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
    my $rule = Pugs::Compiler::Regex->compile( 'ab|c', { ratchet => 1, p => 0 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( !$match, "basic alternative" );
    $match = $rule->match("ab");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "basic alternative - 2" );
}

{
    my $rule = Pugs::Compiler::Token->compile( 'ab|ac' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "alternation no backtracking" );
}

{
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Token->compile( '((.).)(.)' )->code;
    #print "Source: ", do{use Data::Dumper; Dumper(Pugs::Compiler::Rule->compile( '((.).)(.)', { ratchet => 1 } )->{perl5})};
    my $match = test->rule_method( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xyz", 'named rules are methods' );
}

{
    # calling named subrules
    *test::rule_method3 = Pugs::Compiler::Token->compile( '.' )->code;
    *test::rule_method4 = Pugs::Compiler::Token->compile( '<rule_method3>' )->code;
    #print "Source: ", do{use Data::Dumper; Dumper(Pugs::Compiler::Token->compile( '<rule_method3>' )->{perl5})};
    my $match = test->rule_method4( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in same grammar' );
}

{
    # calling named subrules in other grammars
    *test2::rule_method = Pugs::Compiler::Token->compile( '.', { ratchet => 1 } )->code;
    *test::rule_method5 = Pugs::Compiler::Token->compile( '<test2.rule_method>', { ratchet => 1 } )->code;
    my $match = test->rule_method5( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in other grammar' );
}

{
    # calling unnamed subrules
    $test2::rule2 = Pugs::Compiler::Rule->compile( '.' );
    *test::rule_method2 = Pugs::Compiler::Token->compile( '<$test2::rule2>', { ratchet => 1 } )->code;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a global unnamed subrule' );
}

{
    # calling unnamed subrules
    my $match;
    my $rule2 = Pugs::Compiler::Token->compile( '.', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule2->perl5 )};
    eval {
    *test::rule_method6 = Pugs::Compiler::Token->compile( '<$rule2>', { ratchet => 1 } )->code;
    $match = test->rule_method6( "xyzw" );
    };
    warn "# *** Please check if CPAN module 'PadWalker' is properly installed\n",
         "# *** This is the resulting error: $@"
        if $@;
    #print "Source: ", do{use Data::Dumper; Dumper( Pugs::Compiler::Token->compile( '<$rule2>', { ratchet => 1 } )->perl5 )};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a lexical unnamed subrule' );
}

{
    # generated rules
    my $rule = Pugs::Compiler::Token->compile( '<alpha>+', { ratchet => 1 } );
    my $match = $rule->match( "xy12" );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 ) };
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xy", 'built-in rule <alpha>' );
}

{
    # not-special chars
    my $rule = Pugs::Compiler::Token->compile( ',', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 )};
    my $match = $rule->match( "," );
    is( "$match", ",", 'comma is not a special char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\(', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    my $match = $rule->match( "(xy12)" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "(", 'escaped char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\n', { ratchet => 1 } );
    my $match = $rule->match( "\nxy12" );
    is( "$match", "\n", 'escaped char \\n' );
}

#TODO: 
{
    #local $TODO = "usage of :!p in token is not specified";
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\d', { ratchet => 1 } );
    my $match = $rule->match( "abc123" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "1", 'escaped char \\d' );
}

#TODO: 
{
    #local $TODO = "usage of :!p in token is not specified";
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\D', { ratchet => 1 } );
    my $match = $rule->match( "123abc" );
    is( "$match", "a", 'escaped char \\D' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\d', { ratchet => 1 } );
    my $match = $rule->match( "123" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "1", 'escaped char \\d' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\D', { ratchet => 1 } );
    my $match = $rule->match( "abc" );
    is( "$match", "a", 'escaped char \\D' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\N', { ratchet => 1 } );
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
    my $rule = Pugs::Compiler::Token->compile( 'a?bg?', { ratchet => 1 } );
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
    my $rule = Pugs::Compiler::Token->compile('some (text) { return { a => $_[0][0] ,} } ', { ratchet => 1 });
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
    my $rule = Pugs::Compiler::Token->compile('some (text) { return { a => $() ,} } ', { ratchet => 1 });
    my $match = $rule->match("sometext");
    #print Dumper($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}

{
    # alternation
    my $rule = Pugs::Compiler::Token->compile('[a|b](b)', { ratchet => 1 } );
    my $match = $rule->match( "bb" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "bb", 'alternation' );
}

{
    # basic named capture
    my $rule = Pugs::Compiler::Token->compile('$<cap> := (<ws>)', { ratchet => 1 } );
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
    my $rule = Pugs::Compiler::Token->compile('a<ws>', { ratchet => 1 } );
    my $match = $rule->match( "a b" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a ", 'named capture from subrule' );
    is( $match->{'ws'}, " ", 'named capture - 2' );
}

{
    # before
    my $rule = Pugs::Compiler::Token->compile('a<before b>', { ratchet => 1 } );
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
    my $rule = Pugs::Compiler::Token->compile('a$', { ratchet => 1 } );
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
    my $rule = Pugs::Compiler::Token->compile('a..', { ratchet => 1 } );
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
    my $rule1 = Pugs::Compiler::Token->compile('<after xyz>a', { ratchet => 1 } );
    my $rule = Pugs::Compiler::Token->compile('...<$rule1>', { ratchet => 1 } );
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

{
    # quantified capture ?
    my $rule = Pugs::Compiler::Token->compile('a(b)?c', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match->[0][0], undef, 'quantifier ? empty match' );
    
    $match = $rule->match( "abc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][0]", "b", 'quantifier ? one match' );

    $match = $rule->match( "abbbc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'quantifier ? no match' );
}

{
    # quantified capture *
    my $rule = Pugs::Compiler::Token->compile('a(b)*c', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match->[0][0], undef, 'quantifier * empty match' );
    
    $match = $rule->match( "abc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][0]", "b", 'quantifier * one match' );

    $match = $rule->match( "abbbc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][1]", "b", 'quantifier * many match' );

    $match = $rule->match( "abbbd" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'quantifier * no match' );
}

{
    # quantified capture +
    my $rule = Pugs::Compiler::Token->compile('a(b)+c', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'quantifier + no match' );
    
    $match = $rule->match( "abc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][0]", "b", 'quantifier + one match' );

    $match = $rule->match( "abbbc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][1]", "b", 'quantifier + many match' );

    $match = $rule->match( "abbbd" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'quantifier + no match' );
}

{
    # S05 example
    my $rule = Pugs::Compiler::Token->compile(
           #   $0--     $1------
           #   |   |    |       |
           ' [ (\w+) \: (\w+ \ *)* \n ]* ',
        { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $text = "foo:food fool\nbar:bard barb\n";
    my $match = $rule->match( $text );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    
    #       [ Match.new(str=>'foo'), Match.new(str=>'bar') ]
    #
    # and $1 contains the equivalent of:
    #
    #       [ Match.new(str=>'food '),
    #         Match.new(str=>'fool' ),
    #         Match.new(str=>'bard '),
    #         Match.new(str=>'barb' ),
    #       ]
    #print "Match: ", do{use Data::Dumper; Dumper(@a)};
    is( 0+@{$match->[0]}, 2, 'non-capturing with captures inside' );
    is( 0+@{$match->[1]}, 4, 'non-capturing with captures inside - 2' );
    
    my $a = join( ",", @{$match->[0]} );
    my $b = join( ",", @{$match->[1]} );
    #print "Match: ", do{use Data::Dumper; Dumper(@a)};
    is( $a, 'foo,bar', 'non-capturing with captures inside - 3' );
    is( $b, 'food ,fool,bard ,barb', 'non-capturing with captures inside - 4' );
}

{
    # S05 example
    my $rule = Pugs::Compiler::Token->compile(
           # $0-----------------------
           # |                        |
           # | $0[0]    $0[1]---      |
           # | |   |    |       |     |
       '     ( (\w+) \: (\w+ \ *)* \n )*   ',
        { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $text = "foo:food fool\nbar:bard barb\n";
    my $match = $rule->match( $text );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    
     # Because it's in a quantified capturing block,
     # $0 contains the equivalent of:
     #
     #       [ Match.new( str=>"foo:food fool\n",
     #                    arr=>[ Match.new(str=>'foo'),
     #                           [
     #                               Match.new(str=>'food '),
     #                               Match.new(str=>'fool'),
     #                           ]
     #                         ],
     #                  ),
     #         Match.new( str=>'bar:bard barb',
     #                    arr=>[ Match.new(str=>'bar'),
     #                           [
     #                               Match.new(str=>'bard '),
     #                               Match.new(str=>'barb'),
     #                           ]
     #                         ],
     #                  ),
     #       ]
     #
     # and there is no $1
     
    #print "Match: ", do{use Data::Dumper; Dumper(@a)};
    is( 0+@{$match->[0]}, 2, 'capturing with captures inside' );
    is( $match->[1], undef, 'capturing with captures inside - 2' );
    
    is( "$match->[0][0]", "foo:food fool\n", 'capturing with captures inside - 3' );
    is( "$match->[0][1]", "bar:bard barb\n", 'capturing with captures inside - 4' );
}

{
    # sigspace
    
    # XXX - double <ws> doesn't work
    #my $rule = Pugs::Compiler::Rule->compile('a (b) * c', { ratchet => 1, s => 1 } );
    
    my $rule = Pugs::Compiler::Rule->compile('a (b )*c' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    
    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'sigspace no match' );

    $match = $rule->match( "a c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a c", 'sigspace match' );
    is( $match->[0][0], undef, 'sigspace empty match' );
    
    $match = $rule->match( "a b c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][0]", "b ", 'sigspace one match' );

    $match = $rule->match( "a b b b c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][1]", "b ", 'sigspace many match' );

    $match = $rule->match( "a b b b d" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'sigspace no match' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := (.)(.)' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on parentheses' );
    is( "$match->[0]", "b", 'named capture on parentheses not positioned' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := [.](.)' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on square brackets' );
    is( "$match->[0]", "b", 'named capture on square brackets not positioned' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := <any>(.)' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on subrule' );
    is( "$match->[0]", "b", 'named capture on subrule not positioned' );
}


{
    my $match;
    Pugs::Compiler::Rule->install('Test::rule1' => 'xxyy'),  
    Pugs::Compiler::Rule->install('Test::rule2' => 'abc'),   
    @Test::test = (
        'Test::rule1',  
        'Test::rule2',   
    );   
    $rule1 = Pugs::Compiler::Rule->compile('<@Test::test> 123');
    #print $rule1->perl5;
    $match = $rule1->match("abc 123");
    is($match,'abc 123',"array of rules");
}
