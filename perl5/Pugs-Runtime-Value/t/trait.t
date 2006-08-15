#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
plan tests => 4;

use Pugs::Runtime::Container::Scalar;
use Pugs::Runtime::Value;

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

    # warn $n->id;
    # warn ref( $n->cell );

    my $m = Scalar->new;

    # my $p = Scalar->new;
    # $m->bind($p);
    # warn "m bound to p";

    $m->bind($n);

    # warn "m bound to n";

    # this only works because Scalar.pm has a 'tieable' method:
    ok( $m->tieable, '... bound scalar is tieable too' );
}

