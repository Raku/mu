use v5.10;
use lib '../../src/perl6';
use MooseX::Declare;
BEGIN {
    $ENV{'PERL6LIB'} = '../../src/perl6/';
}
{
    # STD needs to be important from the main package
    package main;
    use STD;
}
BEGIN {do 'viv'};
class Mildew::Frontend::STD {
    method parse($source) {
        VIV::SET_OPT('match'=>1,'pos'=>1);
        $ENV{'STD5PREFIX'} = '../../src/perl6/';
        my $m = STD->parse($source, actions=>'Actions');
        $m->{'_ast'}->emit_m0ld;
    }
}
