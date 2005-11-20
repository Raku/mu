#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::Core::Closure');
use_ok('Perl6::Core::Symbol');

{
    my $p = closure::params->new();
    isa_ok($p, 'closure::params');
    
    is($p->equal_to($p), $bit::TRUE, '... the param is equal to itself');    
}


{
    my $p1 = closure::params->new(
        symbol->new('$.greeting' => 'str')
    );
    isa_ok($p1, 'closure::params');
    
    my $p2 = closure::params->new(
        symbol->new('$.greeting' => 'str')
    );
    isa_ok($p2, 'closure::params');    

    is($p1->equal_to($p2), $bit::TRUE, '... the params are equal with one symbol');    
}

{
    my $p1 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),        
    );
    isa_ok($p1, 'closure::params');

    my $p2 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),                
    );
    isa_ok($p2, 'closure::params');    

    is($p1->equal_to($p2), $bit::TRUE, '... the params are equal with two symbols');    
}


{
    my $p1 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),        
    );
    isa_ok($p1, 'closure::params');

    my $p2 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'), 
        symbol->new('$.song' => 'type'),                        
    );
    isa_ok($p2, 'closure::params');    

    is($p1->equal_to($p2), $bit::FALSE, '... the params are not_equal (as execpted)');    
}


{
    my $p1 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),        
        symbol->new('$.song' => 'type'),  
    );
    isa_ok($p1, 'closure::params');

    my $p2 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),                       
    );
    isa_ok($p2, 'closure::params');    

    is($p1->equal_to($p2), $bit::FALSE, '... the params are not_equal (as execpted)');    
}

{
    my $p1 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'),        
        symbol->new('$.song' => 'type'),  
    );
    isa_ok($p1, 'closure::params');

    my $p2 = closure::params->new(
        symbol->new('$.greeting' => 'str'),
        symbol->new('$.name' => 'symbol'), 
        symbol->new('$.song' => 'list'),                                
    );
    isa_ok($p2, 'closure::params');    

    is($p1->equal_to($p2), $bit::FALSE, '... the params are not_equal (as execpted)');    
}

