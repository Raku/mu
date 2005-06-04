use v6;

############################################################
# Iterative solution by Limbic~Region (Joshua Gatcomb)     #
# + optional functionality in Algorithm::Loops NestedLoops #
############################################################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

sub NestedLoop (++@loop, +$OnlyWhen, +$code) returns Ref{
    my @pos = 0 xx (@loop.elems - 1), -1;
    return sub {
        my @group;
        loop {
            if ++@pos[-1] > @loop[-1].end {
                for reverse 0 .. @pos.end - 1 -> $i {
                    next if @pos[$i] == @loop[$i].end;
                    ++@pos[$i];
                    @pos = (@pos[0..$i], 0 xx (@pos.end - $i)) and last;
                }
            }
            return () if @pos[-1] > @loop[-1].end;
            @group = map -> $i { @loop[$i][@pos[$i]] } 0 .. @pos.end;
            if $OnlyWhen.does(Code) { $OnlyWhen(@group) or next }
            $code(@group) if $code.does(Code);
            last;
        }
        return @group;
    };
};
my $next = NestedLoop(loop => @loops);
my @group;
while @group = $next() { say ~@group; }
