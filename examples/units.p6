#!/usr/bin/pugs

use v6;

class Unit {
    has $.q   is rw; 
    method set    ($self: Num $value) { $.q = $value; $self; };
    method string ($self:) { return $self.q ~ "`" ~ $self.abbreviation }
}

class Distance is Unit { method test () {}; }

class Feet is Distance {
    method abbreviation () { "ft" };
    method to (Distance $newUnit) {
        given $newUnit {
            when .does('Feet') { return Feet.new(:q($.q)           );}
            when .does('Meter'){ return Feet.new(:q($.q * 0.3048)  );}
            default { ... }
        }
    }
}

class Meter is Distance {
    method abbreviation () { "m" };
    method to (Distance $newUnit) {
        given $newUnit {
            when .does('Meter') { return Meter.new(:q($.q)         );}
            when .does('Feet')  { return Meter.new(:q($.q / 0.3048));}
            default { ... }
        }
    }
}

sub ft ()     returns Unit { Feet.new() };
# not m, as that's taken by m//
sub M ()      returns Unit { Meter.new()};

multi sub *prefix:<~> (Unit $unit) { $unit.string; }
multi sub *infix:<`>   (Int $value, Unit $unit) { $unit.set($value); };

    
multi sub *infix:<+>   (Distance $a, Distance $b) {
   my $new = $a.clone;
#   return $new.set($a.q + $b.to($a).q);
   my $temp = $b.to($a);
   $temp = $temp.q;
   # $new.set( $a.q + $temp.q );
   $new.set( $a.q + $temp );
   return $new;
} 

my $x = 5`ft;
my $y = 6`M;
say $x.string; say $y.string;
my $z = $x.to($y);
say $z.string;

my $add = (1`ft) + (1`M);
say $add.string;

