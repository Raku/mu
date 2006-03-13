
use Test::More tests => 26;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );  # lrep-generated rule parser
use_ok( 'Pugs::Runtime::Rule2' ); # user rule parser
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );
use_ok( 'Pugs::Runtime::Match' );

{
    my $rule = Pugs::Grammar::Rule::rule( '((.).)(.)' );
    my $src = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    $rule2 = eval $src;
    die $@ if $@;
    my $match = Match->new( $rule2->( "xyzw" ) );
    #print "rule: $src";
    #print 'whole match: ', do{use Data::Dumper; Dumper($match)};
    #print 'whole match [0]: ', do{use Data::Dumper; Dumper($match->[0])};
    #print 'whole match [0][0]: ', do{use Data::Dumper; Dumper($match->[0][0])};
    is( eval { "$match" }, "xyz", 'stringify 1' );
    is( eval { "$match->[0]" }, "xy", 'stringify 2' );
    is( eval { "$match->[0][0]" }, "x", 'stringify 3' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '((.).)' );
    my $src = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    $rule2 = eval $src;
    die $@ if $@;
    my $match = Match->new( $rule2->( "xyz" ) );
    #print "rule 2: $src";
    #print 'whole match 2 array(): ', do{use Data::Dumper; Dumper($match->array)};
    #print 'whole match 2[0]: ', do{use Data::Dumper; Dumper($match->[0])};
    #print 'whole match 2: ', do{use Data::Dumper; Dumper($match)};
    is( eval { "$match" }, "xy", 'stringify 1' );
    is( eval { "$match->[0]" }, "xy", 'stringify 2' );
    is( eval { "$match->[0][0]" }, "x", 'stringify 3' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '(.)(.)' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    my $ret = ['a', 'b'];
    is_deeply( [@$match], $ret, 'return match 1' );
    is( "$match", "ab", 'return match 2' );
    is( "$match->[0]", "a", 'return match 3' );
    is( "$match->[1]", "b", 'return match 4' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '..' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "xyz" ) );
    is( "$match", "xy", 'stringify' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '$z := (.) { return { x => { %{$_[0]} } ,} } ' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );

=for debugging
    print 'whole match: ', do{use Data::Dumper; Dumper($match)};
    my $ret1 =  
        {
            'x' => [
                  {
                    'z' => [
                             'a'
                           ]
                  }
            ]
        };
    is_deeply( $match->(), $ret1, 'return match' );
=cut

    ok( $match, 'true match' );

    my $ret = $match->();
    is( $ret->{x}{z}, "a", 'returns correct struct' );

}

{
    my $rule = Pugs::Grammar::Rule::rule( '$x := (.)  $y := (.)');
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "123" ) );
    my $ret = { x => '1', y => '2' };
    is_deeply( {%$match}, $ret, 'return match' );
    is( "$match", "12", 'stringify' );
    is( "$match->{x}", "1", 'hashify' );
    is( "$match->{y}", "2", 'hashify' );
    is( 0+$match, 12, 'numify' );
}

use_ok( 'Pugs::Compiler::Rule' );
{
    my $rule = Pugs::Compiler::Rule->compile( 'b' );
    my $match = $rule->match( "b" );
    is( $match?1:0, 1, 'boolean true');    
    $match = $rule->match( "x" );
    is( $match?1:0, 0, 'boolean false');    
}
