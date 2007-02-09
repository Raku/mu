use v6-alpha;
use Test;
plan 10;

# L<S29/Num/"=item roots">

my $EPS = 0.0000001;

sub has_approx(Num $n, @list){
    for @list->my $i {
        if abs($i - $n) < $EPS {
            return 1;
        }
    }
    return undef;
}

{
    my @l = eval('roots(-1, 2)');
    ok(!$!, 'roots($x, $n) compiles', :todo<feature>);
    ok(@l.elems == 2, 'roots(-1, 2) returns 2 elements', :todo<feature>);
    ok(has_approx(1i, @l), 'roots(-1, 2) contains 1i', :todo<feature>);
    ok(has_approx(-1i, @l), 'roots(-1, 2) contains -1i', :todo<feature>);
}

{
    my @l = eval('16.roots(4)');
    ok(!$!, '$x.roots($n) compiles', :todo<feature>);
    ok(@l.elems == 2, 'roots(16, 4) returns 4 elements', :todo<feature>);
    ok(has_approx(2, @l), 'roots(16, 4) contains 2', :todo<feature>);
    ok(has_approx(2i, @l), 'roots(16, 4) contains 2i', :todo<feature>);
    ok(has_approx(-2, @l), 'roots(16, 4) contains -2', :todo<feature>);
    ok(has_approx(-2i, @l), 'roots(16, 4) contains -2i', :todo<feature>);
}
    
