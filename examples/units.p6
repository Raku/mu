#!/usr/bin/pugs

use v6;

class Unit {
    has $.q   is rw; 

    method set    ($self: Num $value) { $.q = $value; $self; };
    multi method string ($self:) {$self.q ~ $self.abbreviation; } 
    multi method string ($self: Num $prec) {
        return $self.q.as('%.' ~ $prec ~ 'f') ~ $self.abbreviation;
    }

    method toBase($self:)     { $.q * $self.baseFactor }
    method fromBase($self:)   { $.q / $self.baseFactor }
   
    method setFromBase($self: Num $value) { $.q = $value / $self.baseFactor; }    
    method baseFactor() { 1; }       
}

multi sub *infix:<+>   (Unit $a, Unit $b) {
   die unless $a.type eq $b.type;
   my $new = $a.clone();
   # wierd hack around a wierd error.
   my $aVal = $a.toBase();
   my $bVal = $b.toBase();
   $new.setFromBase( $aVal + $bVal);
   return $new;
}

multi sub *infix:<->   (Unit $a, Unit $b) {
   my $new = $a.clone();
   # wierd hack around a wierd error.
   my $aVal = $a.toBase();
   my $bVal = $b.toBase();
   $new.setFromBase( $aVal - $bVal);
   return $new;
} 

class Distance is Unit { method type () { 'distance'} }

multi sub postfix:<ft> (Num $value) { Feet.new().set($value)  }
class Feet is Distance {
    method abbreviation () { "ft" };
    method baseFactor   () { 0.3048 };
}

# not m, as that's taken by m//
multi sub postfix:<m> (Num $value) { Meter.new().set($value)  }
class Meter is Distance {
    method abbreviation () { "m" };
    # no base factor because it IS the base Unit
}

multi sub postfix:<km> (Num $value) { Kilometer.new().set($value)  }
class Kilometer is Distance {
    method abbreviation () { "km" };
    method baseFactor    () { 1000 };
}

multi sub prefix:<~> (Unit $unit)   { $unit.string; }
#multi sub infix:<`>  (Int $value, Unit $unit) { $unit.set($value); };

my $feet  = 5ft;
my $meter = 6m;
say $feet.string();
say $meter.string();
# my $z = $x.to($y);
# say $z.string;

my $add = $feet + $meter;
say $add.string(2);

my $add2 = $meter + $feet;
say ~$add2;

my $sub = $meter - $feet;
say ~$sub;

my $kilometer = 1000km;
$add = $kilometer + 90ft; 
say $add.string(2);

my $add = 1ft + 1m;
say $add.string(2);

