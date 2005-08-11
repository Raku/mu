#!/usr/bin/pugs

use v6;
use Test;

=kwid 

.isa() tests

These tests are specific to the .isa() which is attached to the
Perl6 Array "class". Which is actually @array.meta.isa(), which 
is actually just the normal OO .isa(). This test does not attempt
to test anything other than the "normal" behavior of @array.isa()

Further clarification of .isa() can be found here:

http://www.nntp.perl.org/group/perl.perl6.language/20974

=cut

plan 19;

{ # normal function call notation  
    my @arr = <1 2 3 4>;
    
    ok(isa(@arr, 'Array'), '... @arr is-a Array');
    ok(isa(@arr, 'List'), '... @arr is-also-a List');
    
    # check a failing case
    ok(!isa(@arr, 'Hash'), '... @arr is-not-a Hash');
}

{ # invocant notation  
    my @arr = <1 2 3 4>;
    
    ok(@arr.isa('Array'), '... @arr is-a Array (invocant notation)');
    ok(@arr.isa('List'), '... @arr is-also-a List (invocant notation)');
    
    # check a failing case
    ok(!@arr.isa('Hash'), '... @arr is-not-a Hash (invocant notation)');
}

{ # normal function call notation   
    my $arr_ref = <1 2 3 4>;
    
    ok(isa($arr_ref, 'Array'), '... $arr is-a Array');
    ok(isa($arr_ref, 'List'), '... $arr is-also-a List');

    # check a failing case
    ok(!isa($arr_ref, 'Hash'), '... $arr is-not-a Hash');      
}

{ # invocant notation   
    my $arr_ref = <1 2 3 4>;
    
    ok($arr_ref.isa('Array'), '... $arr is-a Array (invocant notation)');
    ok($arr_ref.isa('List'), '... $arr is-also-a List (invocant notation)');

    # check a failing case
    ok(!$arr_ref.isa('Hash'), '... $arr is-not-a Hash (invocant notation)');      
}

# check error cases

dies_ok { isa() }, '... isa() dies without any arguments';

{
    my @arr = <1 2 3 4>;
    dies_ok { isa(@arr)  }, '... isa() with a single arg is a failing case';
    dies_ok { @arr.isa() }, '... isa() with a single arg is a failing case (invocant notation)';  
      
    dies_ok { isa(@arr, 'Array', 'Hash') }, '... isa() with a extra args is a failing case';  
    dies_ok { @arr.isa('Array', 'Hash')  }, '... isa() with a extra args is a failing case (invocant notation)';        
}

## some edge cases, and weirdness

{ # check .isa() on inline values

    ok([1, 2, 3, 4].isa('Array'), '... [1, 2, 3, 4].isa("Array") works');
    ok(![1, 2, 3, 4].isa('Hash'), '... [1, 2, 3, 4].isa("Hash") fail predicably');    
}
