#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 3;

use Perl6::Container::Scalar;
use Perl6::Value;

role Mytrait => {
    methods => {
        'tieable' => sub { 
            _('$:cell')->{tieable} = 1; 
            1;
        }, 
        'tie' =>     sub { 
            shift; 
            _('$:cell')->{tieable} = 1; 
            _('$:cell')->tie(@_); 
        },
        'untie' =>   sub { 
            _('$:cell')->untie;
        },
    }
};

role Moretrait => {
    methods => {
        'more' => sub { 
            1;
        }, 
    }
};

{
    my $anon_class = class 'Anon'.rand => {
        is => [ 'Scalar' ], 
        does => [ 'Mytrait', 'Moretrait' ],
    };
    my $n = $anon_class->new;
    ok( $n->does('Mytrait'), 'my Scalar does Mytrait' );
    is_deeply( [ $n->does ], [ 'Mytrait', 'Moretrait' ], '... get list of traits' );
    ok( $n->tieable, '... is tieable' );
}

