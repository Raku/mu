use v6;

sub prompt (?$prompt) {
    print "$prompt ";
    my $temp; ($temp= =<>).chomp;
    say "";
    return $temp;
};

sub cls {
    system(($?OS eq any<MSWin32 mingw>) ?? 'cls' :: 'clear');
}

class Weapon {
    has Str $.name          is rw;
    has Int $.powerLow      is rw;
    has Int $.powerHigh     is rw;

    method damage () {
        return int(rand($.powerHigh - $.powerLow + 1) + $.powerLow);
    };
}

class Mortal {
    has Str     $.name      is rw;
    has Int     $.life      is rw;
}

class Person is Mortal {
    has Str     $.location  is rw;
    has Weapon  %.weapons   is rw;
            
    method where () {
        return "You are currently in the $.location";
    };

    method battle_choice (Monster $enemy) {
        my $choice;

        say "";
        say $enemy.name, " is attacking you! What will you do?";

        until ($choice eq 'f' or $enemy.dead) {
            for %.weapons.kv -> $key, $wep {
                say "\t$key-attack with $wep.name()"
            }
            say "\tf-flee in terror!";
            $choice = prompt("Your choice?");
            given $choice {
                cls;
                when 'f' {
                    say "You ran away from the $enemy.name()!"; 
                }
                if (%.weapons.exists($_)) {
                    ./attack($enemy, %.weapons{$_});
                }
                else {
                    say "Please enter a valid command!"
                }
            }
        }
        say "The $enemy.name() is dead!" unless $choice eq 'f';
    }
      
    method attack (Monster $enemy, Weapon $weapon) {
        say "You attack the $enemy.name() with your $weapon.name()!";
        $enemy.hit($weapon.damage);
        $enemy.attack($_);
        say "";
        say "Your health: $.life\t$enemy.name(): $enemy.life()";
    }

    method hit  ($power) { $.life -= $power; $.life = 0 if $.life < 0; }
    method dead ()       { $.life <= 0 }
    
}

class Monster is Mortal {
    has Int    $.gold   is rw;
    has Weapon $.weapon is rw;

    method hit  ($power) { $.life -= $power; $.life = 0 if $.life < 0; }
    method dead ()       { $.life <= 0 };
    method attack (Person $human) {
        say "$.name attacks $human.name() with $.weapon.name()!";
        $human.hit($.weapon.damage);
    };
}

my $person = Person.new(:life(100));
$person.weapons<a> = Weapon.new(:name<sword>, :powerLow(3), :powerHigh(5) );
$person.weapons<s> = Weapon.new(:name<spell>, :powerLow(0), :powerHigh(7) );

my $wep = Weapon.new(:name<froggers>, :powerLow(3), :powerHigh(5));
my $enemy  = Monster.new(:name("Army of frogs"), :gold(int rand 100), :life(50),
                         :weapon($wep) );

$person.location = "Lobby";
cls;
$person.name = prompt("What is your name:");
say "Greetings, $person.name()!";
say $person.where;
$person.battle_choice($enemy);
