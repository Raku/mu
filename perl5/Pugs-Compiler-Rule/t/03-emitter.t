
use Test::More tests => 1;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );
use_ok( 'Pugs::Grammar::Rule::Lib' );  # old p5 code... -- move 'use' to P:G:R
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );

{
    my $rule = Pugs::Grammar::Rule::rule( '.' );
    print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit();
    print "emit: ", $str, "\n";
}
{
    my $rule = Pugs::Grammar::Rule::rule( '(.)' );
    print Dumper( $rule->{capture} );
    my $str = Pugs::Emitter::Rule::Perl5::emit();
    print "emit: ", $str, "\n";
}
