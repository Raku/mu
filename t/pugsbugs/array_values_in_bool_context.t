#!/usr/bin/pugs

use v6;
require Test;

plan 14;

##  array checking  ##

{
    my @array = (1,0,"true","","1","0",undef);
    
    my ($a,$b,$c,$d,$e,$f,$g);
    
    $a=$b=$c=$d=$e=$f=$g=0;
    
    $a = 1 if @array[0];
    $b = 1 if @array[1];
    $c = 1 if @array[2];
    $d = 1 if @array[3];
    $e = 1 if @array[4];
    $f = 1 if @array[5];
    $g = 1 if @array[6];
    
    ok($a==1, 'array in bool context - numeric true value');
    ok($b!=1, 'array in bool context - numeric false value');
    ok($c==1, 'array in bool context - string true value');
    ok($d!=1, 'array in bool context - string false value');
    ok($e==1, 'array in bool context - stringified true value');
    ok($f!=1, 'array in bool context - stringified false value');
    ok($g!=1, 'array in bool context - undef value');
}

##  hash checking  ##

{
    my %hash = (0=>1,1=>0,2=>"true",3=>"",4=>"1",5=>"0",6=>undef);
    
    my ($a,$b,$c,$d,$e,$f,$g);
    
    $a=$b=$c=$d=$e=$f=$g=0;
    
    $a = 1 if %hash{0};
    $b = 1 if %hash{1};
    $c = 1 if %hash{2};
    $d = 1 if %hash{3};
    $e = 1 if %hash{4};
    $f = 1 if %hash{5};
    $g = 1 if %hash{6};
    
    ok($a==1, 'hash in bool context - numeric true value');
    ok($b!=1, 'hash in bool context - numeric false value');
    ok($c==1, 'hash in bool context - string true value');
    ok($d!=1, 'hash in bool context - string false value');
    ok($e==1, 'hash in bool context - stringified true value');
    ok($f!=1, 'hash in bool context - stringified false value');
    ok($g!=1, 'hash in bool context - undef value');
}
