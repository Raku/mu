use v6;

sub prompt (?$prompt) {
    print $prompt;
    my $temp; ($temp= =<>).chomp;
    return $temp;
};

class Weapon {
    has $.name     is rw;
    has $.power    is rw;
}

class Person {
    has $.location is rw;
    has $.name     is rw;
    has $.life     is rw;
    has $.attach   is rw;
    has $.spell    is rw;
    has %.weapons  is rw;
            
    method where () {
        return "You are currently in the $.location";
    };

    method battle_choice (Monster $enemy) {
        my $choice;

        say $enemy.name, " is attatcking you! What will you do?";

        until ($choice eq 'f' or $enemy.dead) {
            for $.weapons.kv -> $key, $wep {
                say "\t$key-attack with $wep.name()"
            }
            say "\tf-flee in terror!";
            given prompt("Your choice?") {
                when 'f' {
                    say "You ran away from the $enemy.name()!"
                }
                if ($.weapons.exists($_)) {
                    .attack($enemy, $.weapons{$_});
                }
                else {
                    say "Please enter a valid command!"
                }
            }
        }

        say "Ths $enemy.name()", " is dead!"
    }
      
    method attack ($enemy, $weapon) {
        say "You attack the $enemy.name()", " with your $weapon.name()!";
        say "Your health: $.life\t$enemy.name(): $enemy.life()";
        $enemy.hit($weapon.power);
    }
}

class Monster {
    has $.name is rw;
    has $.gold is rw;
    has $.life is rw;

    method hit  ($power) { $.life -= $power; }
    method dead ()       { $.life <= 0 }
    method attack  ()    { int rand $.life }
}

my $person = Person.new(:life(100), :attack(1), :spell(2));
$person.weapons<a> = Weapon.new(:name<sword>, :power(5) );
$person.weapons<s> = Weapon.new(:name<spell>, :power(2) );

my $enemy  = Monster.new(:name("Army of frogs"), :gold(int rand 100), :life(50) );

$person.location = "Lobby";
$person.name = prompt("What is your name: ");
say "Hello $person.name()";
say $person.where;
$person.battle_choice($enemy);
