#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::Core::Closure');
use_ok('Perl6::Core::Symbol');

{
    my $sig = closure::signature->new(
        params => closure::params->new(),
    );
    isa_ok($sig, 'closure::signature');

    is($sig->equal_to($sig), $bit::TRUE, '... the sig is equal to itself');    
}


{
    my $sig1 = closure::signature->new(
        params => closure::params->new(),
    );
    isa_ok($sig1, 'closure::signature');
    
    my $sig2 = closure::signature->new(
        params => closure::params->new(),
    );
    isa_ok($sig2, 'closure::signature');    

    is($sig1->equal_to($sig2), $bit::TRUE, '... the sigs are equal to each other');    
}

{
    my $sig1 = closure::signature->new(
        params => closure::params->new(),
        returns => 'list'
    );
    isa_ok($sig1, 'closure::signature');

    my $sig2 = closure::signature->new(
        params => closure::params->new(),
        returns => 'list'        
    );
    isa_ok($sig2, 'closure::signature');    

    is($sig1->equal_to($sig2), $bit::TRUE, '... the sigs are equal to each other');    
}



{
    my $sig1 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str')
        ),
        returns => 'list'
    );
    isa_ok($sig1, 'closure::signature');

    my $sig2 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str')
        ),
        returns => 'list'        
    );
    isa_ok($sig2, 'closure::signature');    

    is($sig1->equal_to($sig2), $bit::TRUE, '... the sigs are equal to each other');    
}

{
    my $sig1 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str')
        ),
        returns => 'list'
    );
    isa_ok($sig1, 'closure::signature');

    my $sig2 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str')
        ),
        returns => 'type'        
    );
    isa_ok($sig2, 'closure::signature');    

    is($sig1->equal_to($sig2), $bit::FALSE, '... the sigs are not equal to each other (as expected)');    
}

{
    my $sig1 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str'),
            symbol->new('@.dogs' => 'list'),            
        ),
    );
    isa_ok($sig1, 'closure::signature');

    my $sig2 = closure::signature->new(
        params => closure::params->new(
            symbol->new('$.name' => 'str')
        ),
        returns => 'type'        
    );
    isa_ok($sig2, 'closure::signature');    

    is($sig1->equal_to($sig2), $bit::FALSE, '... the sigs are not equal to each other (as expected)');    
}
