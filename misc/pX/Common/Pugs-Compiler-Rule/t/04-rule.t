
use Test::More tests => 16;

use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

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
    my $rule = Pugs::Compiler::Rule->compile( '((.).)(.)' );
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
    # named rules are methods
    *test::rule_method = Pugs::Compiler::Rule->compile( '((.).)(.)' )->code;
    my $match = test->rule_method( "xyzw" );
    is( "$match", "xyz", 'named rules are methods' );
}

{
    # calling named subrules
    *test::rule_method3 = Pugs::Compiler::Rule->compile( '.' )->code;
    *test::rule_method4 = Pugs::Compiler::Rule->compile( '<rule_method3>' )->code;
    my $match = test->rule_method4( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in same grammar' );
}

{
    # calling named subrules in other grammars
    *test2::rule_method = Pugs::Compiler::Rule->compile( '.' )->code;
    *test::rule_method5 = Pugs::Compiler::Rule->compile( '<test2.rule_method>' )->code;
    my $match = test->rule_method5( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a named subrule in other grammar' );
}

{
    # calling unnamed subrules
    $test2::rule2 = Pugs::Compiler::Rule->compile( '.' );
    *test::rule_method2 = Pugs::Compiler::Rule->compile( '<$test2::rule2>' )->code;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls a global unnamed subrule' );
}

{
    # calling unnamed subrules
    my $match;
    eval {
    my $rule2 = Pugs::Compiler::Rule->compile( '.' );
    *test::rule_method6 = Pugs::Compiler::Rule->compile( '<$rule2>' )->code;
    $match = test->rule_method6( "xyzw" );
    };
    warn "# *** Please check if CPAN module 'PadWalker' is properly installed\n",
         "# *** This is the resulting error: $@"
        if $@;
    is( "$match", "x", 'a named subrule calls a lexical unnamed subrule' );
}

{
    # generated rules
    my $rule = Pugs::Compiler::Rule->compile( '<alpha>+' );
    my $match = $rule->match( "xy12" );
    is( "$match", "xy", 'built-in rule <alpha>' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\(' );
    my $match = $rule->match( "(xy12)" );
    is( "$match", "(", 'escaped char' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\n' );
    my $match = $rule->match( "\nxy12" );
    is( "$match", "\n", 'escaped char \\n' );
}

{
    # escaped chars
    my $rule = Pugs::Compiler::Rule->compile( '\N' );
    my $match = $rule->match( "\n\n" );
    is( "$match", "", 'escaped char \\N' );
    $match = $rule->match( "xy12" );
    is( "$match", "x", 'escaped char \\N #2' );
}
