
use Test::More tests => 19;
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
    is( eval { "$match" }, "xyz", 'stringify' );
    #print 'whole match [0]: ', do{use Data::Dumper; Dumper($match->[0])};
    is( eval { "$match->[0]" }, "xy", 'stringify' );
    is( eval { "$match->[0][0]" }, "x", 'stringify' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '((.).)' );
    my $src = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    $rule2 = eval $src;
    die $@ if $@;
    my $match = Match->new( $rule2->( "xyz" ) );
    #print "rule 2: $src";
    #print 'whole match 2: ', do{use Data::Dumper; Dumper($match)};
    is( eval { "$match" }, "xy", 'stringify' );
    is( eval { "$match->[0]" }, "xy", 'stringify' );
    is( eval { "$match->[0][0]" }, "x", 'stringify' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '(.)(.)' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    my $ret = ['a', 'b'];
    is_deeply( [@$match], $ret, 'return match' );
    is( "$match", "ab", 'return match' );
    is( "$match->[0]", "a", 'return match' );
    is( "$match->[1]", "b", 'return match' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '..' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "xyz" ) );
    is( "$match", "xy", 'stringify' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '$z := (.) { return { x => $() ,} } ' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    my $ret =  
        {
            'x' => [
                  {
                    'z' => [
                             'a'
                           ]
                  }
            ]
        };
    is_deeply( $match->(), $ret, 'return match' );
    ok( $match, 'true match' );
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


