
use Test::More tests => 16;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::RegexPerl5' );
use Pugs::Compiler::Rule;
no warnings qw( once );

use Pugs::Runtime::Match; # overload doesn't work without this ???

{
    my $rule = Pugs::Compiler::RegexPerl5->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match?1:0, 1, 'booleanify' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[1]", "x", 'stringify 3' );
    is( "$match->[2]", "z", 'stringify 4' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '((.).)(.)', { Perl5 => 1 } );
    my $match = $rule->match( "xyzw" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( $match?1:0, 1, 'booleanify' );
    is( "$match", "xyz", 'stringify 1' );
    is( "$match->[0]", "xy", 'stringify 2' );
    is( "$match->[1]", "x", 'stringify 3' );
    is( $match->[2]->to, 3, 'to() 4' );
    is( "$match->[2]", "z", 'stringify 4' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(a.)', { Perl5 => 1 } );
    my $match = $rule->match( "xayzwabc", { pos => 5 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "ab", 'pos set' );
    $match = $rule->match( "xayzwabc", { p => 5 } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "ab", 'p set' );
    $match = $rule->match( "xayzwabc", { pos => undef } );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "ay", 'p unset' );
    $match = $rule->match( "xayzwabc" );
    #print "Source: ", do{use Data::Dumper; Dumper($rule->{perl5})};
    #print "Match: ", do{use Data::Dumper; Dumper($match)};
    is( "$match", "ay", 'no pos' );
}

