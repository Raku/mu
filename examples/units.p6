use v6;

class Unit {
    has $.current is rw; #current unit type
    has $.value   is rw; #current unit value
    method set ($self: Int $value) { $.value = $value; $self;   };
    method string ($self:)         { return $.value ~ "`" ~ $.current }
}

class Distance is Unit { }

class Feet is Distance {
    submethod BUILD () { $.current = "f" };
    method to (Distance $newUnit) {
        given $newUnit {
            when .does(Feet) { return $.value;          }
            when .does(Meter){ return $.value * 0.3048; }
        }
    }
}

class Meter is Distance {
    submethod BUILD () { $.current = "m" };
    method to (Unit $newUnit) {
        given $newUnit {
            when .does(Meter) { return $.value;          }
            when .does(Feet)  { return $.value / 0.3048; }
        }
    }
}

sub F () returns Unit { Feet.new() };
sub M () returns Unit { Meter.new()};

multi sub postfix:<~> (Unit $unit) { $unit.string; }
multi sub infix:<`>   (Int $value, Unit $unit) { $unit.set($value); };
multi sub infix:<+>   (Distance $a, Distance $b) {
   warn "HERE";
   return $a.value + $b.to($a);
} 

my $x = 5`F;
my $y = 6`M;
say $x.string; say $y.string;
my $z = $x.to($y);
$z.string.say;
