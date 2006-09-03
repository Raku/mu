
use Test::More tests => 49;
use Data::Dumper;

use_ok( 'Pugs::Compiler::Regex' );
use_ok( 'Pugs::Grammar::Base' );

{
    my $rule = Pugs::Compiler::Regex->compile( '.' );
    #print $rule->{perl5};
    my $match = $rule->match( "xyzw" );
    #print "match: ", $match->perl;
    is( "$match", "x", 'stringify 1' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '.|.' );
    #print $rule->{perl5};
    my $match = $rule->match( "xyzw" );
    #print "match: ", $match->perl;
    is( "$match", "x", 'stringify 2' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '.*' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "xyzw", 'stringify 4' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '.|.|.' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "x", 'stringify 5' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '..|..' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "xy", 'stringify 6' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '.:.' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "xy", 'stringify 7' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)' );
    my $match = $rule->match( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match->data)};
    is( "$match", "x", 'stringify 1' );
    is( $match->(), "x", 'stringify 1' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    #print "Match: ", do{use Data::Dumper; Dumper($match->data)};
    is( "$match", "xyz", 'stringify 1' );
    is( $match->(), "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '((.).)' );
    my $match = $rule->match( "xyz" );
    is( "$match", "xy", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)(.)' );
    my $match = $rule->match( "abc" );
    my $ret = ['a', 'b'];
    is_deeply( [@$match], $ret, 'return match 1' );
    is( "$match", "ab", 'return match 2' );
    is( "$match->[0]", "a", 'return match 3' );
    is( "$match->[1]", "b", 'return match 4' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '..' );
    my $match = $rule->match( "xyz" );
    is( "$match", "xy", 'concat stringify' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '$<z> := (.) { return { x => { %{$_[0]} } ,} } ' );
    print Dumper( Pugs::Grammar::Rule->rule( 'x' )->() );
    # print $rule->{perl5};
    my $match = $rule->match( "abc" );
    #print "match: ", $match->perl;
    ok( $match, 'true match' );
    my $ret = $match->();
    is( $ret->{x}{z}, "a", 'returns correct struct' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '$<z> := [.] { return { x => { %{$_[0]} } ,} } ' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    my $ret = $match->();
    is( $ret->{x}{z}, "a", 'returns correct struct' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '$<z> := <any> { return { x => { %{$_[0]} } ,} } ' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    my $ret = $match->();
    is( $ret->{x}{z}, "a", 'returns correct struct' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '$<x> := (.)  $<y> := (.)');
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    my $match = $rule->match( "123" );
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    my $ret = { x => '1', y => '2' };
    is_deeply( {%$match}, $ret, 'return match' );
    is( "$match", "12", 'stringify' );
    is( "$match->{x}", "1", 'hashify' );
    is( "$match->{y}", "2", 'hashify' );
    is( 0+$match, 12, 'numify' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( 'b' );
    my $match = $rule->match( "b" );
    is( $match?1:0, 1, 'boolean true');    
    is( $match->from, 0, 'match->from');
    is( $match->to, 1, 'match->to');
    $match = $rule->match( "xby" );
    is( $match?1:0, 1, 'boolean true (non-anchored match)');    
    is( $match->from, 1, 'match->from');
    is( $match->to, 2, 'match->to');
    $match = $rule->match( "x" );
    is( $match?1:0, 0, 'boolean false');    
}

# Now, try from and to on P::C::Rule instead of P::C::Regex:
use_ok( 'Pugs::Compiler::Rule' );
use_ok( 'Pugs::Runtime::Match' );
{
    my $rule = Pugs::Compiler::Rule->compile( 'b' );
    #print $rule->{perl5};
    my $match = $rule->match( "b" );
    is( $match?1:0, 1, 'boolean true');    
    is( $match->from, 0, 'match->from');
    is( $match->to, 1, 'match->to');
    $match = $rule->match( "xby" );
    is( $match?1:0, 1, 'boolean true (non-anchored match)');  
  #TODO: {  
  #  local $TODO = "non-achored match breaks from/to";
    is( $match->from, 1, 'match->from');
    is( $match->to, 2, 'match->to');
  #}
    $match = $rule->match( "x" );
    is( $match?1:0, 0, 'boolean false');    
}
