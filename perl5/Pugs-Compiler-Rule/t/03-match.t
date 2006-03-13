
use Test::More tests => 26;
use Data::Dumper;

use_ok( 'Pugs::Compiler::Rule' );

{
    my $rule = Pugs::Compiler::Rule->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '((.).)' );
    my $match = $rule->match( "xyz" );
    is( "$match", "xy", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[0][0]", "x", 'stringify 3' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '(.)(.)' );
    my $match = $rule->match( "abc" );
    my $ret = ['a', 'b'];
    is_deeply( [@$match], $ret, 'return match 1' );
    is( "$match", "ab", 'return match 2' );
    is( "$match->[0]", "a", 'return match 3' );
    is( "$match->[1]", "b", 'return match 4' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '..' );
    my $match = $rule->match( "xyz" );
    is( "$match", "xy", 'stringify' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '$z := (.) { return { x => { %{$_[0]} } ,} } ' );
    my $match = $rule->match( "abc" );
    ok( $match, 'true match' );
    my $ret = $match->();
    is( $ret->{x}{z}, "a", 'returns correct struct' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( '$x := (.)  $y := (.)');
    my $match = $rule->match( "123" );
    my $ret = { x => '1', y => '2' };
    is_deeply( {%$match}, $ret, 'return match' );
    is( "$match", "12", 'stringify' );
    is( "$match->{x}", "1", 'hashify' );
    is( "$match->{y}", "2", 'hashify' );
    is( 0+$match, 12, 'numify' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( 'b' );
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
