use v5.10;
use MooseX::Declare;
use lib '../../src/perl6';
do 'viv';
use STD;
class Mildew::Parser {
    method parse($source) {
        my $m = STD->parse($source, actions=>'Actions');
        $m->{'_ast'};
    }
}
