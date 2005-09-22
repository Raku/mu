#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 60;
use Test::Exception;

use Perl6::MetaModel;

=pod

This example is not yet complete, it is meant to showcase some
Role capability, but I need to get Roles totally working first.
However I hate not having things in version control, so i am 
commiting this.

=cut

# Roles

my $Shape = role 'Shape' => {
    methods => {
        'area' => sub {},
    }
};

my $Shape3D = role 'Shape3D' => {
    does => [ $Shape ],
    methods => {
        'volume' => sub {}, 
    }
};

# Classes

my $Square = class 'Square' => {
    is => [ $::Object ],
    does => [ $Shape ],
    attributes => [ '$.length' ],
    methods => {
        'area' => sub { _('$.length') ** 2 },
        'perimeter' => sub { _('$.length') * 4 },        
        'length' => sub { 
            shift;
            _('$.length' => shift) if @_;
            _('$.length'); 
        }
    }
};

my $Square3D = class 'Square3D' => {
    is => [ $Square ],
    does => [ $Shape3D ],
    methods => {
        'volume' => sub { _('$.length') ** 3  },
        'area'   => sub { ::next_METHOD() * 6 }
    }
};

my $Rectangle = class 'Rectangle' => {
    is   => [ $Square ],
    does => [ $Shape ],
    attributes => [ '$.height' ],
    methods => {
        'area'   => sub { _('$.height') * _('$.length') },
        'perimeter' => sub { (_('$.length') * 2) + (_('$.height') * 2) },
        'height' => sub { 
            shift;
            _('$.height' => shift) if @_;
            _('$.height');
        },      
    }
};

my $Rectangle3D = class 'Rectangle3D' => {
    is => [ $Rectangle ],
    does => [ $Shape3D ],
    attributes => [ '$.width' ],
    methods => {
        'volume' => sub {
            ((_('$.height') * _('$.length')) * 2) +
            ((_('$.height') * _('$.width'))  * 2) +
            ((_('$.width')  * _('$.length')) * 2)            
        },
        'area'   => sub {
            (_('$.width')  * ::next_METHOD())
        },
        'width' => sub { 
            shift;
            _('$.width' => shift) if @_;
            _('$.width');
        }        
    }
};

my $Circle = class 'Circle' => {
    is => [ $::Object ],
    does => [ $Shape ],
    attributes => [ '$.radius' ],
    methods => {
        'area'     => sub { $::CLASS->FETCH('$.PI') * (_('$.radius') ** 2) },
        'diameter' => sub { _('$.radius') * 2 },
        'circumference' => sub { $::CLASS->FETCH('$.PI') * $::SELF->diameter },
        'radius' => sub { 
            shift;
            _('$.radius' => shift) if @_;  
            _('$.radius');
        }        
    }
};

$Circle->STORE('$.PI' => 3.14159);

my $Circle3D = class 'Circle3D' => {
    is => [ $Circle ],
    does => [ $Shape3D ],
    methods => {
        'volume' => sub { (4 / 3) * $Circle->FETCH('$.PI') * (_('$.radius') ** 3) },
        'area'   => sub { 4 * ::next_METHOD() },        
    }
};

{

    ok($Rectangle->does('Shape'), '... the Rectangle class does Shape');
   
    ok($Rectangle3D->does('Shape'), '... the Rectangle3D class does Shape');
    ok($Rectangle3D->does('Shape3D'), '... the Rectangle3D class does Shape3D');    

    ok($Square->does('Shape'), '... the Square class does Shape');    
    
    ok($Square3D->does('Shape'), '... the Square3D class does Shape');    
    ok($Square3D->does('Shape3D'), '... the Square3D class does Shape3D');      
    
    ok($Circle->does('Shape'), '... the Circle class does Shape');    
    
    ok($Circle3D->does('Shape'), '... the Circle3D class does Shape');    
    ok($Circle3D->does('Shape3D'), '... the Circle3D class does Shape3D');          

}


{
    my $rect = $Rectangle->new('$.height' => 10, '$.length' => 10);
    isa_ok($rect, 'Rectangle');

    ok($rect->does('Shape'), '... the rectangle does Shape');

    is($rect->height, 10, '... got the right height');
    is($rect->length, 10, '... got the right length');
    is($rect->area, 100, '... got the right area');
    is($rect->perimeter, 40, '... got the right area');    

    $rect->height(5);

    is($rect->height, 5, '... got the right height');
    is($rect->area, 50, '... got the right area');
    is($rect->perimeter, 30, '... got the right area');    

    $rect->length(2);

    is($rect->length, 2, '... got the right length');
    is($rect->area, 10, '... got the right area');
    is($rect->perimeter, 14, '... got the right area');        
}

{
    my $rect3D = $Rectangle3D->new('$.height' => 4, '$.length' => 8, '$.width' => 3);
    isa_ok($rect3D, 'Rectangle3D');
    
    ok($rect3D->does('Shape'), '... the rectangle does Shape');
    ok($rect3D->does('Shape3D'), '... the rectangle does Shape3D');        

    is($rect3D->length, 8, '... got the right length');
    is($rect3D->height, 4, '... got the right height');
    is($rect3D->width,  3, '... got the right width');      
      
    is($rect3D->volume, 136, '... got the right volume');
    is($rect3D->area, 96, '... got the right area');               
}


{
    my $square = $Square->new('$.length' => 10);
    isa_ok($square, 'Square');
    
    ok($square->does('Shape'), '... the square does Shape');    

    is($square->length, 10, '... got the right length');
    is($square->area, 100, '... got the right area');
    is($square->perimeter, 40, '... got the right area');        

    $square->length(5);

    is($square->length, 5, '... got the right length');
    is($square->area, 25, '... got the right area');
    is($square->perimeter, 20, '... got the right area');    
}

{
    my $square3D = $Square3D->new('$.length' => 2.1);
    isa_ok($square3D, 'Square3D');
    
    ok($square3D->does('Shape'), '... the square does Shape');  
    ok($square3D->does('Shape3D'), '... the square does Shape3D');          

    is($square3D->length, 2.1, '... got the right length');
    is($square3D->area, 26.46, '... got the right area');
    is($square3D->volume, 9.261, '... got the right volume');        

    $square3D->length(5);

    is($square3D->length, 5, '... got the right length');
    is($square3D->area, 150, '... got the right area');
    is($square3D->volume, 125, '... got the right volume');    
}

{
    my $circle = $Circle->new('$.radius' => 4.2);
    isa_ok($circle, 'Circle');

    ok($circle->does('Shape'), '... the circle does Shape');  

    is($circle->radius, 4.2, '... got the right radius');
    is($circle->diameter, 8.4, '... got the right diameter');    
    is($circle->circumference, 26.389356, '... got the right circumference');        
    is($circle->area, 55.4176476, '... got the right area');
}

{
    my $circle3D = $Circle3D->new('$.radius' => 4);
    isa_ok($circle3D, 'Circle3D');

    ok($circle3D->does('Shape'), '... the circle does Shape');  
    ok($circle3D->does('Shape3D'), '... the circle does Shape3D');      

    is($circle3D->radius, 4, '... got the right radius');
    is($circle3D->diameter, 8, '... got the right diameter');    
    is($circle3D->circumference, 25.13272, '... got the right circumference');        
    is($circle3D->area, 201.06176, '... got the right area');
    is($circle3D->volume, 268.082346666667, '... got the right volume');
}
