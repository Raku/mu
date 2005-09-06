#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 39;
use Test::Exception;

use Perl6::MetaModel;

=pod

some of these tests were converted from t/oo/submethods.t in Pugs

=cut

{
    my $was_in_foo_build = 0;
    my $was_in_bar_build = 0;

    my ($Foo, $Bar);
    lives_ok {   
        $Foo = class 'Foo' => { 
            'is' => [ $::Object ],
            'submethods' => {
                'BUILD' => sub { $was_in_foo_build++ } 
            }
        };

        $Bar = class 'Bar' => {
            'is' => [ $Foo ],
            'submethods' => {
                'BUILD' => sub { $was_in_bar_build++ }
            }
        };
    } '... classes were built ok';

    my $a;
    ok($a = $Foo->new(),    "Foo.new() worked (1)");
    is($was_in_foo_build, 1,      "Foo's BUILD was called");
    # is instead of todo_is to avoid unexpected succeedings
    is($was_in_bar_build, 0,      "Bar's BUILD was not called");

    my $b;
    ok($b = $Bar->new(),    "Bar.new() worked");
    is($was_in_foo_build, 2,      "Foo's BUILD was called again");
    is($was_in_bar_build, 1,      "Bar's BUILD was called, too");

    # The next three tests are basically exactly the same as the first three tests
    # (not counting the initial class definition). This is to verify our call to
    # Bar.new didn't removed/changed some internal structures which'd prevent
    # Foo.BUILD of getting called.
    my $c;
    ok($c = $Foo->new(), "Foo.new() worked (2)");
    is($was_in_foo_build, 3,      "Foo's BUILD was called again");
    is($was_in_bar_build, 1,      "Bar's BUILD was not called again");
}

# See thread "BUILD and other submethods" on p6l
# http://groups-beta.google.com/group/perl.perl6.language/msg/e9174e5538ded4a3
{
    my $was_in_baz_submethod  = 0;
    my $was_in_grtz_submethod = 0;

    my ($Baz, $Grtz);
    lives_ok {   
        $Baz = class 'Baz' => { 
            'is' => [ $::Object ],
            'submethods' => {
                'blarb' => sub { $was_in_baz_submethod++ }
            }
        };
        $Grtz = class 'Grtz' => { 
            'is' => [ $Baz ],
            'submethods' => {
                'blarb' => sub { $was_in_grtz_submethod++ }
            }
        };
    } '... our classes built successfully';

    my ($baz, $grtz);
    ok($baz  = $Baz->new,  "Baz.new() worked");
    ok($grtz = $Grtz->new, "Grtz.new() worked");

    eval { $baz->blarb };
    is($was_in_baz_submethod,  1, "Baz's submethod blarb was called");
    # No :todo to avoid unexpected suceedings
    is($was_in_grtz_submethod, 0, "Grtz's submethod blarb was not called");

    eval { $grtz->blarb };
    is($was_in_baz_submethod,  1, "Baz's submethod blarb was not called again");
    # No :todo to avoid unexpected suceedings
    is($was_in_grtz_submethod, 1, "Grtz's submethod blarb was called now");

}

# submethod BUILD with signatures that don't map directly to attributes
{

    my $ClassC = class 'ClassC' => {
        'is' => [ $::Object ],
        'attributes' => [ '$.double_value' ],    
        'methods' => {
            'double_value' => sub { _('$.double_value') }
        },
        'submethods' => {
            'BUILD' => sub {
                my ($self, %params) = @_;  
                $params{'value'} ||= 1;              
                _('$.double_value' => ($params{'value'} * 2));
            }
         }
    };

    my $C = $ClassC->new();
    is($C->double_value, 2, 'BUILD() should allow default values of optional params in signature');

    my $C2 = $ClassC->new(value => 100);
    is($C2->double_value, 200, '... or value passed in');
}

## submethods are not inherited ...
{
    my $Foo2 = class 'Foo2' => {
        'is' => [ $::Object ],    
        'submethods' => {
            'bar' => sub { 'Foo::bar<submethod>' }
        }
    };

    # call the submethod in the direct instance

    my $foo = $Foo2->new();
    isa_ok($foo, 'Foo2');

    can_ok($foo, 'bar');

    {
        my $value;
        lives_ok {
            $value = $foo->bar()
        } '... calling bar() succedded';
        is($value, 'Foo::bar<submethod>', '... got the right return value');    
    }

    # fail calling it from a subclass

    my $Baz2 = class 'Baz2' => { 'is' => [ $Foo2 ] };

    my $baz = $Baz2->new();
    isa_ok($baz, 'Baz2');
    isa_ok($baz, 'Foo2');

    can_ok($baz, 'bar');

    dies_ok {
        $baz->bar()
    } '... calling bar() failed';
}

## submethods are skipped in some cases ....

# single inheritance example ...
=pod

class Foo {
    method baz { ... }
}    

class Bar is Foo {
    submethod baz { ... }
}

class FooBar is Bar {}

my $foo_bar = FooBar.new();
$foo_bar.baz() # calls Foo::baz()

=cut

{

    my $Foo3 = class 'Foo3' => {
        'is' => [ $::Object ],    
        'methods' => {
            'baz' => sub { 'Foo::baz' }
        }
    };

    my $Bar3 = class 'Bar3' => {
        'is' => [ $Foo3 ],
        'submethods' => {
            'baz' => sub { 'Bar::baz<submethod>' }
        }
    };

    my $FooBar3 = class 'FooBar3' => { is => [ $Bar3 ] };

    # now check that the correct method is called

    my $foo_bar = $FooBar3->new();
    isa_ok($foo_bar, 'FooBar3');
    isa_ok($foo_bar, 'Bar3');
    isa_ok($foo_bar, 'Foo3');

    can_ok($foo_bar, 'baz');

    {
        my $value;
        lives_ok {
            $value = $foo_bar->baz()
        } '... calling baz() succedded';
        is($value, 'Foo::baz', '... got the right return value (not submethod)');    
    }    
    
}

# multiple inheritence example
=pod

class Foo {
    method baz { ... }
}

class Bar {
    submethod baz { ... }
}

class FooBar is Foo is Bar {}

my $foo_bar = FooBar.new();
$foo_bar.baz() # calls Foo::baz()

=cut
{
    
    my $Foo4 = class 'Foo4' => {
        'is' => [ $::Object ],    
        'methods' => {
            'baz' => sub { 'Foo::baz' }
        }
    };

    my $Bar4 = class 'Bar4' => {
        'is' => [ $::Object ],    
        'submethods' => {
            'baz' => sub { 'Bar::baz<submethod>' }
        }
    };

    my $FooBar4 = class 'FooBar4' => {
        'is' => [ $Bar4, $Foo4 ]
    };

    # now check that the correct method is called

    my $foo_bar = $FooBar4->new();
    isa_ok($foo_bar, 'FooBar4');
    isa_ok($foo_bar, 'Bar4');
    isa_ok($foo_bar, 'Foo4');

    can_ok($foo_bar, 'baz');

    {
        my $value;
        lives_ok {
            $value = $foo_bar->baz()
        } '... calling baz() succedded';
        is($value, 'Foo::baz', '... got the right return value');    
    }

}