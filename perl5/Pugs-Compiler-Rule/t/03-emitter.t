
use Test::More tests => 1;
use Data::Dumper;

# XXX - move to Rule.p6
use Text::Balanced;
sub Pugs::Grammar::Rule::code {
    return unless $_[0];
    ($extracted,$remainder) = Text::Balanced::extract_codeblock( $_[0] );
    return { 
        bool  => ( $extracted ne '' ),
        match => $extracted,
        tail  => $remainder,
        ( $_[2]->{capture} ? ( capture => [ $extracted ] ) : () ),
    };
}


use_ok( 'Pugs::Grammar::Rule::Engine' );
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
    print Dumper( $rule );
    my $str = Pugs::Emitter::Rule::Perl5::emit();
    print "emit: ", $str, "\n";
}
