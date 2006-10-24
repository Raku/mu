use v6-alpha;
use Test;
plan 3;

#L<S12/Method call vs. Subroutine call>

class test {
    method foo($a) { 'method' }
};
sub foo($a) { 'sub' };
my $obj = test.new;

is foo($obj), 'method', 'if both method and sub with the same name exists the "method" should be called';
is foo $obj , 'method', 'the same as prev but not using brackets';
is foo($obj,),   'sub', 'adding trailing comma should call the "sub"';

