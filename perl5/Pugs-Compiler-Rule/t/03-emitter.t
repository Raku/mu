
use Test::More tests => 4;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );

=for later
{
    my $rule = Pugs::Grammar::Rule::rule( '.' );
    #print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    #print "emit: ", $str, "\n";
    my $rule = eval $str;
    die $@ if $@;
    my $match = $rule->( "abc" );
    print "match: \n", Dumper($match);
}

{
    my $rule = Pugs::Grammar::Rule::rule( '(.)' );
    #print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    #print "emit: ", $str, "\n";
    my $rule = eval $str;
    die $@ if $@;
    my $match = $rule->( "abc" );
    print "match: \n", Dumper($match);
}
=cut

{
    my $rule = Pugs::Grammar::Rule::rule( '$z := (.) { return { x => $() ,} } ' );
    #print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit( $rule->{capture} );
    #print "emit: ", $str, "\n";
    my $rule = eval $str;
    die $@ if $@;
    my $match = $rule->( "abc" );
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
    is_deeply( $match->{capture}, $ret, 'return match' );
}
