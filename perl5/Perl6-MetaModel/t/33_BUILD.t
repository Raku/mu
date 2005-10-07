#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 7;

use Perl6::MetaModel;

=pod

Test to verify that BUILD subroutines run the correct order, 
and override each other as expected. This is a relatively 
simple test, however, what it tests is critical. Also, I am 
sure that other edge cases will come up.

=cut

{
    my $Foo = class 'Foo' => {
        is => [ $::Object ],
        attributes => [ '$:bar' ],
        submethods => {
            'BUILD' => sub {
                _('$:bar' => 'Set in BUILD')
            }
        },
        methods => {
            'bar' => sub { _('$:bar') }
        }
    };
    isa_ok($Foo, 'Foo');

    my $iFoo = $Foo->new('$:bar' => 'Set in new()');
    isa_ok($iFoo, 'Foo');

    is($iFoo->bar(), 'Set in BUILD', '... the attribute was set in BUILD correctly');
    
    my $Bar = class 'Bar' => { 
        is => [ $Foo ],
        submethods => {
            'BUILD' => sub {
                _('$:bar' => 'Set in Bar::BUILD')
            }
        },        
    };
    isa_ok($Bar, 'Bar');    
    isa_ok($Bar, 'Foo');   

    my $iBar = $Bar->new('$:bar' => 'Set in new()');
    isa_ok($iBar, 'Bar');

    is($iBar->bar(), 'Set in Bar::BUILD', '... the attribute was set in Bar::BUILD correctly');     
}
