
use Test::More tests => 5;
use Data::Dumper;

use_ok( 'Pugs::Grammar::Term' );

{
    my $match = Pugs::Grammar::Term->parse( q(...) );
    is_deeply( $match->(), { die => 'not implemented' }, 'yada x 3' );
}

{
    my $match = Pugs::Grammar::Term->parse( q("abc") );
    is( "$match", q("abc"), 'double quoted str' );
}

{
    my $match = Pugs::Grammar::Term->parse( q($abc) );
    is( "$match", q($abc), 'scalar var' );
}

{
    my $match = Pugs::Grammar::Term->parse( q(10) );
    is( "$match", q(10), 'num' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q(
        ( %Pugs::Grammar::Term::hash <?ws>? )*
        # ( <Pugs::Grammar::Term.parse> <?ws>? )*
    ));
    #print $rule->perl5;
    my $match = $rule->match( q(10 $a "abc") );
    my @m = @{$match->[0]};
    #print Dumper( @m );
    is( join(';', map { $_->() } @m), q(10 ;$a ;"abc"), 'split on terms' );
}

