#!/usr/bin/pugs

use v6;
require Test;

plan 30;

=pod

Misc. Junction tests

=cut

{
    # initalize them all to empty strings
    my $a = '';
    my $b = '';
    my $c = '';
    
    # make sure they all match to an empty string
    ok('' eq ($a & $b & $c), 'junction of ($a & $b & $c) matches and empty string');
    ok('' eq all($a, $b, $c), 'junction of all($a, $b, $c) matches and empty string');   
    
    # give $a a value
    $a = 'a';  
    
    # make sure that at least one of them matches 'a' 
    ok('a' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one "a"');
    ok('a' eq any($b, $c, $a), 'junction of any($b, $c, $a) matches at least one "a"');   

    ok('' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one empty string');
    ok('' eq any($b, $c, $a), 'junction of any($b, $c, $a) matches at least one empty string');
    
    # make sure that ~only~ one of them matches 'a'
    ok('a' eq ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    ok('a' eq one($b, $c, $a), 'junction of one($b, $c, $a) matches at ~only~ one "a"');
    
    # give $b a value
    $b = 'a';
    
    # now this will fail
    ok('a' ne ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at more than one "a"');              

    # change $b and give $c a value
    $b = 'b';
    $c = 'c';
    
    ok('a' eq ($b ^ $c ^ $a), 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    ok('b' eq ($a ^ $b ^ $c), 'junction of ($a ^ $b ^ $c) matches at ~only~ one "b"');
    ok('c' eq ($c ^ $a ^ $b), 'junction of ($c ^ $a ^ $b) matches at ~only~ one "c"');  

    ok('a' eq ($b | $c | $a), 'junction of ($b | $c | $a) matches at least one "a"');
    ok('b' eq ($a | $b | $c), 'junction of ($a | $b | $c) matches at least one "b"');
    ok('c' eq ($c | $a | $b), 'junction of ($c | $a | $b) matches at least one "c"'); 
    
    # test junction to junction
    
    ok(('a' | 'b' | 'c') eq ($a & $b & $c), 'junction ("a" | "b" | "c") matches junction ($a & $b & $c)');    
    ok(('a' & 'b' & 'c') eq ($a | $b | $c), 'junction ("a" & "b" & "c") matches junction ($a | $b | $c)'); 
    
    # mix around variables and literals
    
    ok(($a & 'b' & 'c') eq ('a' | $b | $c), 'junction ($a & "b" & "c") matches junction ("a" | $b | $c)');              
    ok(($a & 'b' & $c) eq ('a' | $b | 'c'), 'junction ($a & "b" & $c) matches junction ("a" | $b | "c")');              
    
}

# same tests, but with junctions as variables
{
        # initalize them all to empty strings
    my $a = '';
    my $b = '';
    my $c = '';
    
    my $all_of_them = $a & $b & $c;
    ok('' eq $all_of_them, 'junction variable of ($a & $b & $c) matches and empty string');
    
    $a = 'a';  
    
    my $any_of_them = $b | $c | $a;
    ok('a' eq $any_of_them, 'junction variable of ($b | $c | $a) matches at least one "a"');  
    ok('' eq $any_of_them, 'junction variable of ($b | $c | $a) matches at least one empty string');
    
    my $one_of_them = $b ^ $c ^ $a;
    ok('a' eq $one_of_them, 'junction variable of ($b ^ $c ^ $a) matches at ~only~ one "a"');
    
    $b = 'a';
    
    {
        my $one_of_them = $b ^ $c ^ $a;
        ok('a' ne $one_of_them, 'junction variable of ($b ^ $c ^ $a) matches at more than one "a"');              
    }
    
    $b = 'b';
    $c = 'c';
    
    {
        my $one_of_them = $b ^ $c ^ $a;    
        ok('a' eq $one_of_them, 'junction of ($b ^ $c ^ $a) matches at ~only~ one "a"');
        ok('b' eq $one_of_them, 'junction of ($a ^ $b ^ $c) matches at ~only~ one "b"');
        ok('c' eq $one_of_them, 'junction of ($c ^ $a ^ $b) matches at ~only~ one "c"');  
    }

    {
        my $any_of_them = $b | $c | $a;
        ok('a' eq $any_of_them, 'junction of ($b | $c | $a) matches at least one "a"');
        ok('b' eq $any_of_them, 'junction of ($a | $b | $c) matches at least one "b"');
        ok('c' eq $any_of_them, 'junction of ($c | $a | $b) matches at least one "c"'); 
    }

}







