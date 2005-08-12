#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;

=pod

this test was converted from t/oo/methods/class.t

# L<A12/"Class Methods" /such as a constructor, you say something like:/>

=cut

class 'Foo' => {
    'is' => [ 'Perl6::Object' ],
    'class' => {
        'methods' => { 
            'bar' => sub {
                my ($class, $arg) = @_;
                return 100 + $arg;
            }
        }
    }
};

{
    my $val;
    lives_ok {
        $val = Foo->bar(42);
    } '... class methods work for class';
    is($val, 142, '... basic class method access worked');
}

{
    my $foo = Foo->new();
    dies_ok {
        $foo->bar(42);
    } '... class methods should not work for instances';
}
