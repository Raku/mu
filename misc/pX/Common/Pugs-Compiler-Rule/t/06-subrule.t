use Test::More tests => 18;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

{
    *{'Test123::rule1'} = Pugs::Compiler::Rule->compile('\w')->code();
    *{'Test123::rule2'} = Pugs::Compiler::Rule->compile('(<rule1>)*')->code();
    my $match = Test123->rule2("abc");
    #print Dumper( $$match );
    is($match,'abc',"Matched...");
    is( $match->[0][0],"a","Capture 1...");
    is( $match->[0][1],"b","Capture 2...");
    is( $match->[0][2],"c","Capture 3...");
    is( $match->[0][3],undef,"No more captures");
    is( $match->[1],undef,"No more captures");
}

{
    *{'Test123::rule1'} = Pugs::Compiler::Rule->compile('\w')->code();
    *{'Test123::rule2'} = Pugs::Compiler::Rule->compile('<rule1>*')->code();
    my $match = Test123->rule2("abc");
    is($match,'abc',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[2],"c","Capture 3...");
    is( $match->{rule1}[3],undef,"No more captures");
}

{
    *{'Test123::rule1'} = Pugs::Compiler::Rule->compile('\w')->code();
    *{'Test123::rule2'} = Pugs::Compiler::Rule->compile('<rule1><rule1>')->code();
    my $match = Test123->rule2("abc");
    is($match,'ab',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[3],undef,"No more captures");
}
