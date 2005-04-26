#!/usr/bin/pugs

use v6;
use Test;

plan 21;

##  basic sanity checking  ##

{
    my ($a,$b,$c,$d,$e,$f,$g);
    
    $a = 1 if 1;
    $b = 1 if 0;
    $c = 1 if "true";
    $d = 1 if "";
    $e = 1 if "1";
    $f = 1 if "0";
    $g = 1 if undef;
    
    ok($a==1, 'literal in bool context - numeric true value');
    ok($b==0, 'literal in bool context - numeric false value');
    ok($c==1, 'literal in bool context - string true value');
    ok($d==0, 'literal in bool context - string false value');
    ok($e==1, 'literal in bool context - stringified true value');
    ok($f==0, 'literal in bool context - stringified false value');
    ok($g==0, 'literal in bool context - undef value');
}

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
    ok($b==0, 'array in bool context - numeric false value');
    ok($c==1, 'array in bool context - string true value');
    ok($d==0, 'array in bool context - string false value');
    ok($e==1, 'array in bool context - stringified true value');
    ok($f==0, 'array in bool context - stringified false value');
    ok($g==0, 'array in bool context - undef value');
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
    ok($b==0, 'hash in bool context - numeric false value');
    ok($c==1, 'hash in bool context - string true value');
    ok($d==0, 'hash in bool context - string false value');
    ok($e==1, 'hash in bool context - stringified true value');
    ok($f==0, 'hash in bool context - stringified false value');
    ok($g==0, 'hash in bool context - undef value');
}
