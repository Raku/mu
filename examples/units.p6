use v6;

class Unit {
    has $.value   is rw; #current unit value
    method set ($self: Int $value) { $.value = $value; $self; };
    method string ($self:)         { return $self.value ~ "`" ~ $self.abbreviation }
}

class Distance is Unit { }

class Feet is Distance {
    method abbreviation () { "f" };
    method to (Distance $newUnit) {
        given $newUnit {
            when .does('Feet') { return Feet.new(:value($.value)           );}
            when .does('Meter'){ return Feet.new(:value($.value * 0.3048)  );}
        }
    }
}

class Meter is Distance {
    method abbreviation () { "m" };
    method to (Distance $newUnit) {
        given $newUnit {
            when .does('Meter') { return Meter.new(:value($.value)         );}
            when .does('Feet')  { return Meter.new(:value($.value / 0.3048));}
        }
    }
}

sub F () returns Unit { Feet.new() };
sub M () returns Unit { Meter.new()};

multi sub *postfix:<~> (Unit $unit) { $unit.string; }
multi sub *infix:<`>   (Int $value, Unit $unit) { $unit.set($value); };
multi sub *infix:<+>   (Distance $a, Distance $b) {
   warn "HERE";
   return $a.value + $b.to($a).value;
} 

my $x = 5`F;
my $y = 6`M;
say $x.string; say $y.string;
my $z = $x.to($y);
say $z.string;

my $add = 1`F + 1`M;
say $add.string;

