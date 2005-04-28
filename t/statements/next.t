#!/usr/bin/pugs

use v6;
use Test;

=pod

next if <condition>;
<condition> and next;
next <label>;

=cut

# test for loops with next

{
    my $tracker = 0;
    eval 'for 1 .. 5 {
        next if $_ < 3;          
        $tracker = $_;
    }';
    is($tracker, 5, '... nothing until 3 (next if <cond>)');
}

{
    my $tracker = 0;
    eval 'for 1 .. 5 {
        $_ < 3 && next;  
        $tracker = $_;
    }';
    is($tracker, 5, '... nothing until 3 (<cond> && next)');
}

{
    my $tracker = 0;
    eval 'for 1 .. 5 {
        $_ < 3 and next;          
        $tracker = $_;
    }';
    is($tracker, 5, '... nothing until 3 (<cond> and next)');
}