use v6;

##################################################
# Iterative p5 solution by Roy Johnson           #
# Ported to p6 by Limbic Region (Joshua Gatcomb) # 
##################################################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

sub ret_iter4 (@loops is copy) returns Ref {
    my $last = [*] @loops.map:{ $_.elems };
    my $iter = -1;
    return sub {
        my $i = ++$iter;
        return () if $iter >= $last;
        my $possible = $last;
        return @loops.map:{
            $possible /= $_.elems;
            my $this_iter_i = $i / $possible;
            $i %= $possible;
            $_[$this_iter_i];
        };
    };
}
my $next = ret_iter4(@loops);
my @group;
while @group = $next() { say ~@group; }
