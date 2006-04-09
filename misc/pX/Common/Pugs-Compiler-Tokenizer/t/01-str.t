
use Test::More tests => 4;

use_ok( 'Pugs::Grammar::Str' );

{
    my $match = Pugs::Grammar::Str->parse( q("abc") );
    is( "$match", q("abc"), 'double quoted str' );
}

{
    my $match = Pugs::Grammar::Str->parse( q('abc') );
    is( "$match", q('abc'), 'single quoted str' );
}

{
    my $rule = Pugs::Compiler::Rule->compile( q(
        %Pugs::Grammar::Str::hash
    ));
    #print $rule->perl5;
    my $match = $rule->match( q('abc') );
    is( "$match", q('abc'), 'hash dispatch' );
}

