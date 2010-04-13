use v5.10;
use MooseX::Declare;
use lib '../../src/perl6';
BEGIN {do 'viv'};
class Mildew::Parser::STD {
    method parse($source) {
        VIV::SET_OPT('match'=>1,'pos'=>1);
        my $m = STD->parse($source, actions=>'Actions');
        $m->{'_ast'};
    }
}
