
use Test::More tests => 5;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );
use_ok( 'Pugs::Runtime::Match' );

{
    my $rule = Pugs::Grammar::Rule::rule( '$z := (.) { return { x => $() ,} } ' );
    #print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    #print "emit: ", $str, "\n";
    my $rule = eval $str;
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    #print "match: \n", Dumper($match);
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
    is_deeply( $match->capture, $ret, 'return match' );
    ok( $match->bool, 'true match' );
}

{
    my $rule = Pugs::Grammar::Rule::rule( '(.)(.)' );
    #print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    #print "emit: ", $str, "\n";
    my $rule = eval $str;
    die $@ if $@;
    my $match = Match->new( $rule->( "abc" ) );
    print "match: \n", Dumper($match);
    print "match array: \n", Dumper($match->array);
    #is_deeply( $match->capture, $ret, 'return match' );
}
