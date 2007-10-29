
#use Smart::Comments;
use Test::More tests => 166;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Token' );
use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

use Pugs::Runtime::Match; # overload doesn't work without this ???

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
    #print "Source: ", $rule->perl;
    #print "Match: ", $match->perl;
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->{0}", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
    is( "$match->[1]", "z", 'stringify 4' );
}

{
    # colon is a no-op
    my $rule = Pugs::Compiler::Token->compile( '((.) : .) : (.)' );
    my $match = $rule->match( "xyzw" );
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'stringify 1' );
}

{
    # <null> is a no-op
    my $rule = Pugs::Compiler::Token->compile( '((.) <null> .) <null> (.)' );
    my $match = $rule->match( "xyzw" );
    is( $match?1:0, 1, '<null>' );
    is( "$match", "xyz", 'stringify 1' );
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
    my $rule = Pugs::Compiler::Regex->compile( '[(a)(b)|(c)](d)', { ratchet => 1, p => 0 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( !$match, "alternative with captures" );
    $match = $rule->match("cd");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "alternative - 2" );
    is( $match->[0], "c", "alternative - 3" );
    is( $match->[2], "d", "alternative - 4" );
}

# L<S05/Unchanged syntactic features/
#   "syntax of" "|" not change "semantics do change slightly">
{
    my $rule = Pugs::Compiler::Token->compile( 'ab|ac' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("ac");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    ok( $match, "alternation no backtracking" );
    is $match, 'ac';
}

TODO: {
    local $TODO = "'|' is now with longest-token semantics";
    my $rule = Pugs::Compiler::Token->compile( 'ab|abc' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match("abc");
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    #ok( $match, "alternation no backtracking" );
    is $match, 'abc';
}

# L<S05/Grammars/"subs are the model for rules">
{
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Token->compile( '((.).)(.)' )->code;
    #print "Source: ", do{use Data::Dumper; Dumper(Pugs::Compiler::Rule->compile( '((.).)(.)', { ratchet => 1 } )->{perl5})};
    my $match = test->rule_method( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xyz", 'named rules are methods' );
}

# L<S05/Extensible metasyntax (C<< <...> >>)/
#   "A leading alphabetic character" means "capturing grammatical assertion">
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

# L<S05/Extensible metasyntax (C<< <...> >>)/
#   "A leading $" "an indirect subrule">
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

### XXX built-in subrule <alpha> not formally specified in S05
{
    # generated rules
    my $rule = Pugs::Compiler::Token->compile( '<alpha>+', { ratchet => 1 } );
    my $match = $rule->match( "xy12" );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 ) };
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "xy", 'built-in rule <alpha>' );
    is( join( ' ', $match->keys ), "alpha", 'keys() method' );
    # is( join( ' ', $match->kv ), "alpha xy", 'kv() method' );
}

# L<S05/Simplified lexical parsing/
#   "not all non-identifier glyphs are currently meaningful">
# XXX fix the following test?
{
    # not-special chars
    my $rule = Pugs::Compiler::Token->compile( ',', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper( $rule->perl5 )};
    my $match = $rule->match( "," );
    is( "$match", ",", 'comma is not a special char' );
}

# L<S05/Unchanged syntactic features/"Backslash escape" "\">
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

{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\d', { ratchet => 1 } );
    my $match = $rule->match( "abc123" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "1", 'escaped char \\d' );
}

{
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

# L<S05/Backslash reform/"\N" "negation of \n">
{
    # escaped chars
    my $rule = Pugs::Compiler::Token->compile( '\N', { ratchet => 1 } );
    my $match = $rule->match( "\n\n" );
    is( "$match", "", 'escaped char \\N' );
    $match = $rule->match( "xy12" );
    is( "$match", "x", 'escaped char \\N #2' );
}

# L<S05/Unchanged syntactic features/"Repetition quantifiers" "?">
{
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

# L<S05/Match objects/"you can override that by calling return inside a regex">
# L<S05/Match objects/"you normally just want to return a result object instead">
{
    # capture
    my $rule = Pugs::Compiler::Token->compile('some (text) { return { a => $_[0][0] ,} } ', { ratchet => 1 });
    #print "Source: ", do{use Data::Dumper; Dumper($rule->perl5)};
    my $match = $rule->match("sometext");
    #print "Match: ", do{use Data::Dumper; Dumper($match->data)};
    my $capture = $match->();
    is(ref($capture),'HASH','Capture is a hashref');
    is($capture->{a},'text','$capture->{a}');
}

{
    # capture
    my $rule = Pugs::Compiler::Token->compile('some (text) { return { a => $() ,} } ', { ratchet => 1 });
    my $match = $rule->match("sometext");
    #print Dumper($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}

{
    # capture in empty rule
    my $rule = Pugs::Compiler::Token->compile(
        ' { return { a => "sometext" ,} } ',
        { ratchet => 1, p => 0 });
    #print $rule->{perl5};
    my $match = $rule->match("");
    #print Dumper($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}

# L<S05/Unchanged syntactic features/"Alternatives: " "|">
{
    # alternation
    my $rule = Pugs::Compiler::Token->compile('[a|b](b)', { ratchet => 1 } );
    my $match = $rule->match( "bb" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "bb", 'alternation' );
    is( $match->[0], 'b' );
}

# L<S05/Named scalar aliasing to subpatterns>
{
    # basic named capture
    my $rule = Pugs::Compiler::Token->compile('$<cap> := (<ws>)', { ratchet => 1 } );
    my $match = $rule->match( " " );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", " ", 'named capture' );
    is( $match->{'cap'}, " ", 'named capture - 2' );
    TODO: {
        local $TODO = 'subpattern?';
        is( $match->{'cap'}->{'ws'}, ' ' );
        #is( $match->()->{'cap'}->{'ws'}, ' ' );
    }
}

{
    # basic named capture
    my $rule = Pugs::Compiler::Token->compile('$<cap> := (<alpha>)', { ratchet => 1 } );
    my $match = $rule->match( "a" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a", 'named capture' );
    is( $match->{'cap'}, "a", 'named capture - 2' );
    TODO: {
        local $TODO = 'subpattern again?';
        is( $match->{'cap'}->{'alpha'}, "a" );
    }
}

# L<S05/Subrule captures>
{
    # basic named capture
    my $rule = Pugs::Compiler::Token->compile('a<ws>', { ratchet => 1 } );
    my $match = $rule->match( "a b" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a ", 'named capture from subrule' );
    is( $match->{'ws'}, " ", 'named capture - 2' );
}

# L<S05/"Extensible metasyntax (C<< <...> >>)"/
#    "<before pattern>" "(?=pattern)">
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

}

# L<S05/"Extensible metasyntax (C<< <...> >>)"/
#   "leading !" "negated meaning">
{
    # not-before
    my $rule = Pugs::Compiler::Token->compile('a<!before b>', { ratchet => 1 } );
    my $match = $rule->match( "ac" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a", 'not before' );

    $match = $rule->match( "ab" );
    #print "Source: ", $rule->{perl5};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'before' );
}

{
    # not-alpha
    my $rule = Pugs::Compiler::Token->compile('a<!alpha>.', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'negated alpha' );

    $match = $rule->match( "a!" );
    #print "Source: ", $rule->{perl5};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a!", 'negated alpha matches' );
}

# L<S05/Changed metacharacters/
#    "^ and $" "always match" "start/end of" string>
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

# L<S05/Changed metacharacters/
#    dot matches "any character including newline">
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

# L<S05/"Extensible metasyntax (C<< <...> >>)"/
#    "<after pattern>" "(?<=pattern)">
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
    #print "Source: ", $rule->perl;
    #print "Match: ", $match->perl;
    is( "$match", "", 'not after' );

    # TODO: <!after b>
}

# L<S05/"Quantified subpattern captures">
{
    # quantified capture ?
    my $rule = Pugs::Compiler::Token->compile('a(b)?c', { ratchet => 1 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};

    my $match = $rule->match( "ac" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    # $match->[0]: $0 in Perl 6
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

# L<S05/"Indirectly quantified subpattern captures">
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

# L<S05/"Indirectly quantified subpattern captures"/
#     "In contrast" "outer quantified structure is a capturing" structure>
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

# L<S05/Modifiers/":s" "(:sigspace) modifier" "whitespace sequences" "significant">
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

    $match = $rule->match( "a  \t c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "a  \t c", 'sigspace match' );
    is( $match->[0][0], undef, 'sigspace empty match (multi-sp)' );

    $match = $rule->match( "a b c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][0]", "b ", 'sigspace one match' );

    $match = $rule->match( "a b b b c" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match->[0][1]", "b ", 'sigspace many match' );

    $match = $rule->match( "a b b bc" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match->[0][1], undef, 'sigspace required' );

    $match = $rule->match( "a b b b d" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "", 'sigspace no match' );
}

# L<S05/Named scalar aliasing to subpatterns>
{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := (.)(.)' );
    #print "Rule: ", $rule->{perl5};
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on parentheses' );
    is( "$match->[0]", "b", 'named capture on parentheses not positioned' );

    #print Dumper( $match->[0]->from ), "\n";
    #print Dumper( $match->[0]->to ), "\n";
}

# L<S05/Named scalar aliases applied to non-capturing brackets>
{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := [.](.)' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on square brackets' );
    is( "$match->[0]", "b", 'named capture on square brackets not positioned' );
}

# L<S05/Named scalar aliasing to subrules>
{
    my $rule = Pugs::Compiler::Rule->compile( '$<z> := <any>(.)' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    is( "$match->{z}", "a", 'named capture on subrule' );
    is( "$match->[0]", "b", 'named capture on subrule not positioned' );
}

# L<S05/Extensible metasyntax (C<< <...> >>)/
#   "A leading @" "matches like a bare array" except element treated as subrule>
{
        my $match;
    #Pugs::Compiler::Rule->install('Test::rule1' => 'xxyy'),
    #Pugs::Compiler::Rule->install('Test::rule2' => 'abc'),
    @Test::test = (
        Pugs::Compiler::Token->compile('xxyy'),
        Pugs::Compiler::Token->compile('abc'),
    );
    $rule1 = Pugs::Compiler::Rule->compile('<@Test::test> 123');
    #print $rule1->perl5;
    $match = $rule1->match("abc 123");
    is($match,'abc 123',"array of rules");
}

{
    my $match;
    #Pugs::Compiler::Token->install('Test::rule3' => 'xxyy'),
    #Pugs::Compiler::Token->install('Test::rule4' => 'abc'),
    @Test::test = (
        Pugs::Compiler::Token->compile('xxyy'),
        Pugs::Compiler::Token->compile('abc'),
    );
    $rule1 = Pugs::Compiler::Token->compile('<@Test::test> 123');
    #print $rule1->perl5;
    $match = $rule1->match("abc123");
    is($match,'abc123',"array of tokens");
}

# L<S05/Variable (non-)interpolation/
#   "interpolated hash" inserting "run-time table-driven submatching">
{
=for docs - see S05
    *   An interpolated hash matches the longest possible key of the hash as
        a literal, or fails if no key matches. (A "" key will match
        anywhere, provided no longer key matches.)
    *   If the corresponding value of the hash element is a closure, it is executed.
    *   If it is a string or rule object, it is executed as a subrule.
    *   If it has the value 1, nothing special happens beyond the match.
    *   Any other value causes the match to fail.
=cut
    my $match;
    my $v = 0;
    %Test123::test = (
        if =>    2,        # fail (number, not '1')
        iff =>   1,        # match (longer than 'if')
        until => Pugs::Compiler::Token->compile('(a.a)'),
                           # subrule - match "until(aa)"
        use =>   sub { $v = 1 },
                           # closure - print "use()"
    );
    $rule1 = Pugs::Compiler::Token->compile('%Test123::test 123');

    $match = $rule1->match("iff123");
    is($match,'iff123',"Matched hash{iff}");

    $match = $rule1->match("if123");
    is($match,'',"fail hash{if} - value != 1");

    is($v,0,"closure not called yet");
    $match = $rule1->match("use");
    is($v,1,"closure was called hash{use}");

    $match = $rule1->match("untilaba123");
    #print Dumper($match->data);
    is($match,'untilaba123',"subrule hash{until}");
    is($match->(),'untilaba123',"subrule hash{until} - 2");

}

# L<S05/Extensible metasyntax (C<< <...> >>)/
#    "A leading %" "matches like a bare hash" except string treated as a subrule>
{
    my $match;
    my $v = 0;
    %Test123::test = (
        if =>    2,        # fail (number, not '1')
        iff =>   1,        # match (longer than 'if')
        until => Pugs::Compiler::Token->compile('
            (a.a)
            {
                print " # KEY = $::_V6_MATCH_->{KEY} \n";
                # print " # ", Dumper( $::_V6_MATCH_->data );
                return 42;
            }
        '),
                           # subrule - match "until(aa)"
        use =>   sub { $v = 1 },
                           # closure - print "use()"
        '' =>    Pugs::Compiler::Token->compile('other'),
                           # default subrule - match "other"
    );
    $rule1 = Pugs::Compiler::Token->compile('<%Test123::test> 123');
    #print "<<< ", Pugs::Compiler::Token->compile('<%Test123::test> 123')->{perl5}, ">>>";

    $match = $rule1->match("iff123");
    is($match,'iff123',"Matched hash{iff}");
    #print Dumper( $match->{test}->data );
    is($match->{'Test123::test'},'',"Matched hash{iff} capture");

    $match = $rule1->match("if123");
    is($match,'',"fail hash{if} - value != 1");

    is($v,0,"closure not called yet");
    $match = $rule1->match("use");
    is($v,1,"closure was called hash{use}");

    $match = $rule1->match("untilaba123");
    is($match,'untilaba123',"subrule hash{until}");
    is($match->(),'untilaba123',"subrule hash{until} - 2");

    # is($match->{test},'aba',"Matched hash{until} capture");
    is("" . $match->{'Test123::test'}, 42, "Matched hash{until} capture handles stringification");

    #print "\$/ ",Dumper($match->data);
    #print "\$/{test} ",Dumper($match->{test}->data);
    is( ${ $match->{'Test123::test'} }, 42, "Matched hash{until} return object");

    $match = $rule1->match("other123");
    is($match,'other123',"default subrule");

}

# L<S05/Variable (non-)interpolation/
#   "interpolated hash" inserting "run-time table-driven submatching">
{
    my $match;
    %Test123::test = (
        rule1 => Pugs::Compiler::Token->compile('xx %Test123::test yy'),
        rule2 => Pugs::Compiler::Token->compile('abc'),
    );
    $rule1 = Pugs::Compiler::Token->compile('%Test123::test 123');
    #print $rule1->perl5;
    $match = $rule1->match("rule1xxrule2abcyy123");
    is($match,'rule1xxrule2abcyy123',"Matched hash inside hash");
}

# L<S05/Simplified lexical parsing/
#   "single quotes make everything inside them literal">
{
    my $rule = Pugs::Compiler::Rule->compile( q(
        '>>'
    ) );
    #print $rule->perl;
    #print Dumper( Pugs::Grammar::Rule->rule( " '>>' " )->() );
    my $match = $rule->match( "abc>>zzz" );
    is( "$match", ">>", 'literal ">>"' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q(
        '::'
    ) );
    #print $rule->perl;
    my $match = $rule->match( "abc::zzz" );
    is( "$match", "::", 'literal ":"' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q(
        '}'
    ) );
    #print $rule->perl;
    my $match = $rule->match( "abc}zzz" );
    is( "$match", "}", 'literal "}"' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q^
        ')'
    ^ );
    #print $rule->perl;
    my $match = $rule->match( "abc)zzz" );
    is( "$match", ")", 'literal ")"' );
}

# L<S05/Subpattern numbering/"index of capturing parentheses restarts after" "|">
{
    my $rule = Pugs::Compiler::Token->compile( q(
        (a)
        [
        |  (a) (x) b c
        |  (a) x x y
        ]
    ) );
    #print $rule->perl;
    my $match = $rule->match( "aaxxy" );
    #print Dumper( $match );
    ### $match
    my @a = @{$match};
    TODO: {
        local $TODO = "failing array capture rollback";
        is( scalar @a, 2, 'alternation array rollback' );
    }
    is( $a[0], 'a' );
    is( $a[1], 'a' );
}

{
    my $rule = Pugs::Compiler::Token->compile( q(
        [
        |  <alpha> x b c
        |  <alpha> x x y
        ]
    ) );
    #print $rule->perl;
    my $match = $rule->match( "aaxxy" );
    #print Dumper( $match );
    my %h = %{$match};
    is( scalar keys %h, 1, 'alternation hash rollback' );
    is( $h{alpha}, 'a' );
}

# L<S05/Repeated captures of the same subrule>
TODO:
{
    local $TODO = "failing <alpha> capture";

    my $rule = Pugs::Compiler::Token->compile( q(
        <alpha>
        [
        |  <alpha> x b c
        |  <alpha> x x y
        ]
    ) );
    #print $rule->perl;
    my $match = $rule->match( "aaxxy" );
    #print Dumper( $match );
    my %h = %{$match};
    is( scalar keys %h, 2, 'alternation hash rollback with multiple captures' );
    is( $h{alpha}->[0], 'a' );
    is( $h{alpha}->[1], 'a' );
}

# L<S05/Extensible metasyntax (C<< <...> >>)/
#   "A leading ?" "not to capture what it matches">
{
    my $rule = Pugs::Compiler::Token->compile( q(
        <?alpha>
    ) );
    #print $rule->perl;
    my $match = $rule->match( "aaxxy" );
    #print Dumper( $match );
    is( ${$match}, 'a', 'non-capturing char class' );
    my %h = %{$match};
    is( scalar keys %h, 0, 'non-capturing char class' );
}

# L<S05/"New metacharacters"/
#   "^^ and $$ match line beginnings and endings">
{
    my $rule = Pugs::Compiler::Token->compile( '^^x' );
    my $match = $rule->match( "\nyx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'at-line-start - not' );

    $match = $rule->match( "\nxy\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-line-start' );

    $match = $rule->match( "xy\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-line-start, pos==0' );
}

{
    my $rule = Pugs::Compiler::Token->compile( 'x$$' );
    my $match = $rule->match( "\nyxz\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'at-line-end - not' );

    $match = $rule->match( "\nyx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-line-end' );

    $match = $rule->match( "\nyx" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-line-end, pos==end' );
}

{
    my $rule = Pugs::Compiler::Token->compile( '^x+$' );
    my $match = $rule->match( "\nyxxz\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'anchored at both sides - not' );

    $match = $rule->match( "xxx" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xxx", 'anchored at both sides' );
}

{
    my $rule = Pugs::Compiler::Token->compile( '^^x+$$' );
    my $match = $rule->match( "\nyxxz\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'anchored at line start/end - not' );

    $match = $rule->match( "yxxz\nxxx\nk" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xxx", 'anchored at line start/end' );
}

# L<S05/Nothing is illegal/
#    "match whatever the prior successful regex matched">
{
    my $rule = Pugs::Compiler::Token->compile( '<ident>' );
    my $match = $rule->match( "\n1xy2\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy2", 'ident' );

  {
    my $rule = Pugs::Compiler::Token->compile( '<prior> \s <prior>' );
    my $match = $rule->match( "\n1xy2 xxx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy2 xxx", 'prior' );
  }

{
    my $rule = Pugs::Compiler::Token->compile( '<prior> \s <prior>' );
    my $match = eval {
        $rule->match( "\n1xy2 xy3 kkk lll\n" )
    } || '';
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy2 xy3 kkk lll", 'prior-prior' );
  }
}

# L<S05/Matching against non-strings/"<.can('scratch')>">
# XXX obsolete
{

    {
        package Test1;
        sub meth { $_[0]{v} eq 'True' }
    }

    #my $rule = Pugs::Compiler::Token->compile( '<.can("meth")>' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};

    #my $object = bless { v => 'True' }, 'Test1';

    #my $match = $rule->match( $object );
    #print "Match: ", $match->perl;
    #is( ( $match ? 1 : 0 ) , 1, 'object matches' );
}

# L<S05/"Extensible metasyntax (C<< <...> >>)"/
#    "match only at a particular StrPos">
{
    my $rule = Pugs::Compiler::Token->compile( 'x <at(1)>' );
    my $match = $rule->match( "xy" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at' );
}

# L<S05/"Extensible metasyntax (C<< <...> >>)"/
#    "<( token" "the start of a result capture">
{
    my $rule = Pugs::Compiler::Token->compile( 'x <(y)> z' );
    my $match = $rule->match( "xyz" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( $$match, "y", 'return capture' );
}

# L<S05/Extensible metasyntax (C<< <...> >>)/
#    "<< token indicates a left word boundary">
{
    my $rule = Pugs::Compiler::Token->compile( '<<xyz' );
    my $match = $rule->match( "axyz xyz" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xyz", '<<xyz' );
    is( $match->from > 1, 1, '<<xyz' );
}
{
    my $rule = Pugs::Compiler::Token->compile( '«xyz' );
    my $match = $rule->match( "axyz xyz" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xyz", '<<xyz' );
    is( $match->from > 1, 1, '<<xyz' );
}
{
    my $rule = Pugs::Compiler::Token->compile( 'xyz»' );
    my $match = $rule->match( "xyza xyz" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xyz", 'xyz>>' );
    is( $match->from > 0 , 1, 'xyz>>' );
}
{
    my $rule = Pugs::Compiler::Token->compile( 'xyz>>' );
    my $match = $rule->match( "xyza xyz" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xyz", 'xyz>>' );
    is( $match->from > 0 , 1, 'xyz>>' );
}

# L<S05/New metacharacters/& "separates conjunctive terms">
{
    my $rule = Pugs::Compiler::Token->compile( 'x & <alpha>' );
    my $match = $rule->match( "xyza xyz" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "x", 'x & <alpha>' );
    #is( $match->from > 0 , 1, 'xyz>>' );
}

# L<S05/Longest-token matching/"|" "represents logical alternation with declarative longest-token semantics">
{
    my $rule = Pugs::Compiler::Token->compile( ' y | z ' );
    my $match = $rule->match( "z" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "z", 'y | z' );
}

# L<S05/Unchanged syntactic features/"Repetition quantifiers">
{
    my $rule = Pugs::Compiler::Token->compile( '[xy]*' );
    my $match = $rule->match( "xyx" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $match->to, 2, 'quantifier-rollback' );
}
{
    my $rule = Pugs::Compiler::Token->compile( 'x || y | z' );
    my $match = $rule->match( "z" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "z", 'x || y | z' );
}
{
    my $rule = Pugs::Compiler::Token->compile( 'x .*? z' );
    my $match = $rule->match( "xabcza" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xabcz", 'x .*? z - non-greedy quantifier' );
}
{
    my $rule = Pugs::Compiler::Token->compile( ' $<abc> := <alpha> $<abc> ' );
    my $match = $rule->match( "xx" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "xx", 'backreference' );
}
{
    my $rule = Pugs::Compiler::Token->compile( ' a <?{ 1 }> b ' );
    my $match = $rule->match( "ab" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "ab", 'boolean closure' );
}
{
    my $rule = Pugs::Compiler::Token->compile( ' a <!{ 1 }> b | a <?{ 1 }> c ' );
    my $match = $rule->match( "ac" );
    #print "Ast: ", do{use Data::Dumper; Dumper($rule->{ast})};
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $$match, "ac", 'negative boolean closure' );
}



