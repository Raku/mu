
use Test::More tests => 12;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );
use_ok( 'Pugs::Runtime::Match' );

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
    my $rule = Pugs::Grammar::Rule::rule( '(.)(.)' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    my $ret = ['a', 'b'];
    is_deeply( [@$match], $ret, 'return match' );
    is( "$match", "ab", 'return match' );
}
{
    my $rule = Pugs::Grammar::Rule::rule( '$x := (.)  $y := (.)');
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "123" ) );
    my $ret = { x => '1', y => '2' };
    is_deeply( {%$match}, $ret, 'return match' );
    is( "$match", "12", 'stringify' );
    is( 0+$match, 12, 'numify' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '..' );
    $rule = eval Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    die $@ if $@;
    my $match = Match->new( $rule->( "xyz" ) );
    is( "$match", "xy", 'stringify' );
}
