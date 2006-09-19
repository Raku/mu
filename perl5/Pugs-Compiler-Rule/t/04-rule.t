
use Test::More tests => 49;
use Data::Dumper;
$Data::Dumper::Indent = 1;

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
    #print "Ast: ", do{use Data::Dumper; Dumper( Pugs::Grammar::Rule->rule( 'xxx' . "\n" . '\n' )->()  )};
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xxx\n", 'constant' );
}

#SKIP: 
{
    #skip "newline parser error", 1;

    # \n is whitespace
    my $rule = Pugs::Compiler::Regex->compile( '\n x' . "\n" . '\n' );
    my $match = $rule->match( "\nx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "\nx\n", 'constant' );
}

{
    # unnamed rules are objects
    my $rule = Pugs::Compiler::Regex->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    #print "Source: ", $rule->{perl5};
    #print "Match: ", $match->perl;
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
    is( "$match->[1]", "z", 'stringify 4' );
}

{
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Regex->compile( '((.).)(.)' )->code;
    #print "Code\n";
    my $match = test->rule_method( "xyzw" );
    #print "Match: ", $match->perl;
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
    #print "Source: ", do{use Data::Dumper; Dumper( Pugs::Compiler::Regex->compile( '.' )->{perl5} )};
    #print "Source: ", do{use Data::Dumper; Dumper( Pugs::Compiler::Regex->compile( '<rule_method3>' )->{perl5} )};
    #print "Source: ", do{use Data::Dumper; Dumper( Pugs::Grammar::Rule->rule( '<rule_method3>' )->() )};
    my $match = test->rule_method4( "xyzw" );
    #print "Match: ", $match->perl;
    is( "$match", "x", 'a named subrule calls a named subrule in same grammar' );
}

{
    # calling named subrules in other grammars
    *test2::rule_method = Pugs::Compiler::Regex->compile( '.' )->code;
    *test::rule_method5 = Pugs::Compiler::Regex->compile( '<test2.rule_method>' )->code;
    my $match = test->rule_method5( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'a named subrule calls a named subrule in other grammar' );
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
    # XXX - is $() working? -- now it's called '$$match'
    # capture
    my $rule = Pugs::Compiler::Regex->compile('
        some (text) 
        { 
            #print $_[0]->perl; 
            return { a => $() ,} 
        } ');
    #print "rule: ", $rule->perl;
    my $match = $rule->match("sometext");
    #print Dumper($match);
    my $capture = $match->();
    is($capture->{a},'sometext','simple capture');
}

{
    # calling unnamed subrules
    $test2::rule2 = Pugs::Compiler::Regex->compile( '.' );
    #print "Source [1]: ", $test2::rule2->perl;
    *test::rule_method2 = Pugs::Compiler::Regex->compile( '<$test2::rule2>' )->code;
    #print "Source 'test::rule_method2': ", Pugs::Compiler::Regex->compile( '<$test2::rule2>' )->perl;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", $test2::rule2->perl;
    #print "Match: ", $match->perl;
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
    my $rule = Pugs::Compiler::Regex->compile( '^x' );
    my $match = $rule->match( "\nx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'at-start - not' );

    $match = $rule->match( "x\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-start' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( 'x$' );
    my $match = $rule->match( "\nx\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'at-end - not' );

    $match = $rule->match( "\nx" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "x", 'at-end' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '^^x' );
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
    my $rule = Pugs::Compiler::Regex->compile( 'x$$' );
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
    my $rule = Pugs::Compiler::Regex->compile( '^x+$' );
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
    my $rule = Pugs::Compiler::Regex->compile( '^^x+$$' );
    my $match = $rule->match( "\nyxxz\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'anchored at line start/end - not' );

    $match = $rule->match( "yxxz\nxxx\nk" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xxx", 'anchored at line start/end' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<null>' );
    my $match = $rule->match( "" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "", 'plain null' );

    $match = $rule->match( "xxx");
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( ($match ? 1 : 0 ), 1, 'null but true' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( 'x<null>y' );
    my $match = $rule->match( "xy" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy", 'null between terms' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<ident>' );
    my $match = $rule->match( "\n1xy2\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy2", 'ident' );

  {
    my $rule = Pugs::Compiler::Regex->compile( '<prior>' );
    my $match = $rule->match( "\n1xy2\n" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", $match->perl;
    is( "$match", "xy2", 'prior' );
  }
}

{

    { 
        package Test1;
        sub meth { $_[0]{v} eq 'True' }
    }

    my $rule = Pugs::Compiler::Regex->compile( '<.meth>' );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};

    my $object = bless { v => 'True' }, 'Test1';

    my $match = $rule->match( $object );
    #print "Match: ", $match->perl;
    is( ( $match ? 1 : 0 ) , 1, 'object matches' );
}

