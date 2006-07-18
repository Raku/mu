
use Test::More tests => 29;
use Data::Dump::Streamer;
$Data::Dump::Streamer::Indent = 1;

use_ok( 'Pugs::Compiler::Regex' );
no warnings qw( once );

{
    package test;
    use base Pugs::Grammar::Base;
}

{
    package test2;
    use base Pugs::Grammar::Base;
}

#SKIP: 
{
    #skip "alpha constant parser error", 1;

    # alpha constant
    my $rule = Pugs::Compiler::Regex->compile( 'xxx' . "\n" . '\n' );
    my $match = $rule->match( "xxx\n" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( "$match", "xxx\n", 'constant' );
}

#SKIP: 
{
    #skip "newline parser error", 1;

    # \n is whitespace
    my $rule = Pugs::Compiler::Regex->compile( '\n x' . "\n" . '\n' );
    my $match = $rule->match( "\nx\n" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( "$match", "\nx\n", 'constant' );
}

{
    # unnamed rules are objects
    my $rule = Pugs::Compiler::Regex->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
    is( "$match->[1]", "z", 'stringify 4' );
}

{
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Regex->compile( '((.).)(.)' )->code;
    my $match = test->rule_method( "xyzw" );
    is( "$match", "xyz", 'named rules are methods' );
}

{
    # Install a new rule inside the test package.
    package test;
    Pugs::Compiler::Regex->install( installed_rule => '((.).)(.)' );
    my $match = test->installed_rule( "xyzw" );
    ::is( "$match", "xyz", 'named rules are methods' );
}

{
    # Install a new rule outside the test package.
    Pugs::Compiler::Regex->install( 'test::installed_rule2' => '((.).)(.)' );
    my $match = test->installed_rule2( "xyzw" );
    is( "$match", "xyz", 'named rules are methods' );
}

{
    # Attempt to install an existing rule with an unqualified name.
    package test;
    local $@;
    eval { Pugs::Compiler::Regex->install( installed_rule => '' ) };
    ::like $@, qr/Can't install regex 'installed_rule' as 'test::installed_rule' already exists/,
           'Died as expected when installing an existing unqualified rule';
}

{
    # Attempt to install an existing rule with a fully qualified name.
    local $@;
    eval { Pugs::Compiler::Regex->install( 'test::installed_rule' => '' ) };
    like $@, qr/Can't install regex 'test::installed_rule' as 'test::installed_rule' already exists/,
           'Died as expected when installing an existing qualified rule';
}

{
    # calling named subrules
    *test::rule_method3 = Pugs::Compiler::Regex->compile( '.' )->code;
    *test::rule_method4 = Pugs::Compiler::Regex->compile( '<rule_method3>' )->code;
    my $match = test->rule_method4( "xyzw" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in same grammar' );
}

{
    # calling named subrules in other grammars
    *test2::rule_method = Pugs::Compiler::Regex->compile( '.' )->code;
    *test::rule_method5 = Pugs::Compiler::Regex->compile( '<test2.rule_method>' )->code;
    my $match = test->rule_method5( "xyzw" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in other grammar' );
}

{
    # calling unnamed subrules
    $test2::rule2 = Pugs::Compiler::Regex->compile( '.' );
    *test::rule_method2 = Pugs::Compiler::Regex->compile( '<$test2::rule2>' )->code;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", do{use Data::Dump::Streamer; Dump($rule->{perl5})};
    #print "Match: ", do{use Data::Dump::Streamer; Dump($match)};
    is( "$match", "x", 'a named subrule calls a global unnamed subrule' );
}

{
    # calling unnamed subrules
    my $match;
    eval {
    my $rule2 = Pugs::Compiler::Regex->compile( '.' );
    *test::rule_method6 = Pugs::Compiler::Regex->compile( '<$rule2>' )->code;
    $match = test->rule_method6( "xyzw" );
    };
    warn "# *** Please check if CPAN module 'PadWalker' is properly installed\n",
         "# *** This is the resulting error: $@"
        if $@;
    is( "$match", "x", 'a named subrule calls a lexical unnamed subrule' );
}

{
    # generated rules
    my $rule = Pugs::Compiler::Regex->compile( '<alpha>+' );
    my $match = $rule->match( "xy12" );
    is( "$match", "xy", 'built-in rule <alpha>' );
}

{
    # not-special chars
    my $rule = Pugs::Compiler::Regex->compile( ',' );
    my $match = $rule->match( "," );
    is( "$match", ",", 'comma is not a special char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Regex->compile( '\(' );
    my $match = $rule->match( "(xy12)" );
    is( "$match", "(", 'escaped char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Regex->compile( '\n' );
    my $match = $rule->match( "\nxy12" );
    is( "$match", "\n", 'escaped char \\n' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Regex->compile( '\d' );
    my $match = $rule->match( "abc123" );
    is( "$match", "1", 'escaped char \\d' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Regex->compile( '\D' );
    my $match = $rule->match( "123abc" );
    is( "$match", "a", 'escaped char \\D' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Regex->compile( '\N' );
    my $match = $rule->match( "\n\n" );
    is( "$match", "", 'escaped char \\N' );
    $match = $rule->match( "xy12" );
    is( "$match", "x", 'escaped char \\N #2' );
}

{
    # ambiguous rule /a?bg?/
    # XXX - is this /a? [bg]?/ or /a? b g?/
    # --- It should the same as /a? b g?/
    # 1) spaces should not make difference
    # 2) the other way, it should be as /[a?[bg]]?/
    my $rule = Pugs::Compiler::Regex->compile( 'a?bg?');
    my $match = $rule->match("cdtbprw");
    is("$match","b",'"a?bg?" equals "a? b g?".');
}

{
    # capture
    my $rule = Pugs::Compiler::Regex->compile('some (text) { return { a => $_[0][0]() ,} } ');
    my $match = $rule->match("sometext");
    my $capture = $match->();
    is(ref($capture),'HASH','Capture is a hashref');
    is($capture->{a},'text','$capture->{a}');
}

{
    # XXX - is $() working?
    # capture
    my $rule = Pugs::Compiler::Regex->compile('some (text) { return { a => $() ,} } ');
    my $match = $rule->match("sometext");
    #print Dump($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}
