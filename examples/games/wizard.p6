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
            
      method where () {return "You are currently in the " ~ $.location };
      method battle_choice ($self: $enemy) {
             my $choice;
             say "An " ~ $enemy.name ~ " is attatcking you! What will you do?";             
             until ($choice eq 'f' or $enemy.dead) {
                    my $prompt =  $self.weapons.keys.map:{ my Weapon $wep = $self.weapons(){$_}; "\t" ~ $_ ~ "-attack with " ~ $wep.name() ~ "\n"};
                    $choice = prompt( $prompt ~ "\tf-flee in terror!");
                    given $choice {
#                            when any(keys %$self.weapons) { $self.attack($enemy, $choice);};
                            when 'f' { say "You run like a little girl from the " ~ $enemy.name ~ "!" };
#                            default { say "Please type " ~ join(",", $self.weapons.keys) ~ " or 'f'" }
                    }
             }
      }
      
      method attack ($enemy, $weapon) {
         say "You attack the " ~ $enemy.name ~ " with your " ~ $weapon ~ "!";

         say "Your health: " ~ $.life ~ "\t "~ $enemy.name ~ ":" ~ $enemy.life; 
      }
}

class Monster {
      has $.name is rw;
      has $.gold is rw;
      has $.life is rw;

      method hit  ($power) { $.life -= $power; };
      method dead ()       { $.life > 0 ?? 0 :: 1 };
      method attack  ()    { int rand $.life };
}

my $person = Person.new(:life(100), :attack(1), :spell(2));
$person.weapons{'a'} = Weapon.new(:name("sword"), :power(5) );
$person.weapons{'s'} = Weapon.new(:name("spell"), :power(2) );

my $enemy  = Monster.new(:name("Army of frogs"), :gold(int rand 100), :life(50) );

$person.location = "Lobby";
$person.name = prompt("What is your name: ");
say "Hello " ~ $person.name;
say $person.where;
$person.battle_choice($enemy);
