#!/usr/bin/pugs

use v6;
use Test;

=pod

last if <condition>;
<condition> and last;
last <label>;

=cut

plan 3;

# test for loops with last

{
    my $tracker = 0;
    for 1 .. 5 {
        $tracker = $_;
        last if $_ == 3;  
    }
    is($tracker, 3, '... our loop only got to 3 (last if <cond>)');
}

{
    my $tracker = 0;
    for 1 .. 5 {
        $tracker = $_;
        $_ == 3 && last;  
    }
    is($tracker, 3, '... our loop only got to 3 (<cond> && last)');
}

{
    my $tracker = 0;
    for 1 .. 5 {
        $tracker = $_;
        $_ == 3 and last;  
    }
    is($tracker, 3, '... our loop only got to 3 (<cond> and last)');
}
