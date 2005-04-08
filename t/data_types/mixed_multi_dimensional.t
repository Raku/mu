#!/usr/bin/pugs

use v6;
require Test;

plan 60;

=pod

This tests some mixed multi-dimensional structures.

NOTE:
These tests don't go any more than two levels deep
(AoH, AoP) in most cases because I know these won't
work yet in Pugs. When we have this support, then 
this test should be added too more.

=cut

{ # Array of Pairs
    my @array;
    isa_ok(@array, 'Array');
    
    my $pair = ('key' => 'value');
    isa_ok($pair, 'Pair');
    
    @array[0] = $pair; # assign a variable
    is(+@array, 1, 'the array has one value in it');
    isa_ok(@array[0], 'Pair');
    eval_is('@array[0]<key>', 'value', 'got the right pair value');

    @array[1] = ('key1', 'value1'); # assign it inline
    is(+@array, 2, 'the array has two values in it');
    isa_ok(@array[1], 'Pair');
    eval_is('@array[1]<key1>', 'value1', 'got the right pair value');
}

{ # Array of Hashes
    my @array;
    isa_ok(@array, 'Array');
    
    my %hash = ('key', 'value', 'key1', 'value1');
    isa_ok(%hash, 'Hash');
    is(+%hash.keys, 2, 'our hash has two keys');
    
    @array[0] = %hash;
    is(+@array, 1, 'the array has one value in it');
    isa_ok(@array[0], 'Hash');
    eval_is('@array[0]{"key"}', 'value', 'got the right value for key');
    eval_is('@array[0]<key1>', 'value1', 'got the right value1 for key1');    
}

{ # Array of Lists
    my @array = (1, [2, 3], [4, 5], 6);
    isa_ok(@array, 'Array');
    
    is(+@array, 4, 'got 4 elements in the array of Lists');
    is(@array[0], 1, 'got the right first element');
    isa_ok(@array[1], 'List');
    is(@array[1][0], 2, 'got the right second/first element');    
    is(@array[1][1], 3, 'got the right second/second element');        
    isa_ok(@array[2], 'List');    
    is(@array[2][0], 4, 'got the right third/first element');    
    is(@array[2][1], 5, 'got the right third/second element');            
    is(@array[3], 6, 'got the right fourth element');
}

{ # Array of Subs
    my @array;
    isa_ok(@array, 'Array');
    
    @array[0] = sub { 1 };
    @array[1] = { 2 };
    @array[2] = -> { 3 };
    
    is(+@array, 3, 'got three elements in the list');
    isa_ok(@array[0], 'Sub');
    isa_ok(@array[1], 'Sub');
    isa_ok(@array[2], 'Sub');        
    
    is(@array[0](), 1, 'the first element (when executed) is 1');
    is(@array[1](), 2, 'the second element (when executed) is 2');    
    is(@array[2](), 3, 'the third element (when executed) is 3');
}

{ # Hash of Lists
    my %hash;
    isa_ok(%hash, 'Hash');
    
    %hash<key> = [ 1, 2, 3 ];
    isa_ok(%hash<key>, 'List');
    
    is(+%hash<key>, 3, 'it should have 3 values in it');    
    is(%hash<key>[0], 1, 'got the right value');
    is(%hash<key>[1], 2, 'got the right value');    
    is(%hash<key>[2], 3, 'got the right value');
    
    # detach it from the hash to access it
    {
        my $list := %hash<key>;
        is(+$list, 3, 'it should have 3 values in it');    
        is($list[0], 1, 'got the right value (when I pull the list out)');
        is($list[1], 2, 'got the right value (when I pull the list out)');    
        is($list[2], 3, 'got the right value (when I pull the list out)');    
        
        push($list, 4);
        is(+$list, 4, 'it should now have 4 values in it');
        is($list[3], 4, 'got the right value (which we just pushed onto the list)');    
    }

    # and make sure it was still attached to the %hash
    {
        my $list := %hash<key>;
        is(+$list, 4, 'it should now have 4 values in it (it was still attached)');
    }
}


{ # Hash of Array-refs
    my %hash;
    isa_ok(%hash, 'Hash');
    
    my @array = ( 1, 2, 3 );
    isa_ok(@array, 'Array');
    
    %hash<key> = @array;
    isa_ok(%hash<key>, 'Array');
    
    is(+%hash<key>, 3, 'it should have 3 values in it');       
    is(%hash<key>[0], 1, 'got the right value');
    is(%hash<key>[1], 2, 'got the right value');    
    is(%hash<key>[2], 3, 'got the right value');
    
    # detach it from the hash to access it
    {
        my $array := %hash<key>;
        is(+$array, 3, 'it should have 3 values in it');    
        is($array[0], 1, 'got the right value (when I pull the array out)');
        is($array[1], 2, 'got the right value (when I pull the array out)');    
        is($array[2], 3, 'got the right value (when I pull the array out)');    
        
        push($array, 4);
        is(+$array, 4, 'it should now have 4 values in it');
        is($array[3], 4, 'got the right value (which we just pushed onto the array)');    
    }

    # and make sure it was still attached to the %hash
    {
        my $array := %hash<key>;
        is(+$array, 4, 'it should now have 4 values in it (it was still attached)');
    }    
}
