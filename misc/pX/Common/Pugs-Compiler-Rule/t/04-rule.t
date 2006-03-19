
use Test::More tests => 9;

use_ok( 'Pugs::Compiler::Rule' );

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
    my $rule2 = Pugs::Compiler::Rule->compile( '.' );
    *test::rule_method2 = Pugs::Compiler::Rule->compile( '<$rule2>' )->code;
    my $match = test->rule_method2( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "x", 'a named subrule calls an unnamed subrule' );
}

