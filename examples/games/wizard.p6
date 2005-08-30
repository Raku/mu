use v6;

my $DEBUG = 0;

multi *prompt (?$prompt) {
    print $prompt;
    my $input = =<>; 
    $input  .= chomp;
    return $input;
}

multi *prompt ($prompt, @options is copy) {
    my $i = 0;
    .key //= ++$i for @options;

    my $choice;
#    until ($choice eq any(@options>>.<key>)) {  #not implemented in pugs yet
    until ($choice eq any(@options.map:{ .key }) ) {
        say $prompt;
        say "\t$_.key() $_.text()" for @options;
        $choice = prompt;
    }
     
    my %options_by_key = map { .key => $_; } @options;
    $choice = %options_by_key.{$choice};
    return $choice.param // $choice.key;
}

sub *cls { system(($?OS eq any<MSWin32 mingw>) ?? 'cls' :: 'clear'); }

#random number between $low and $high, ($low..$high).pick but easier on memory
sub *random ($low,$high) {int( rand($high - $low) + $low ) + 1; };
multi sub *infix:<.?.> ($low,$high) {int( rand($high - $low) + $low ) + 1; };

class Option {
    has Str $.key is rw ;
    has Str $.text is rw ;
    has Str $.param is rw ;
}

class wObject {
    has Str $.name     is rw;
    has Str $.location is rw;
    has Str $.last_location is rw;
    has Int $.plural;
    method where () {
        "$.name {$.plural ?? 'are' :: 'is'} currently in the $.location";
    };
}
   
class Weapon is wObject {
    has Int $.power         is rw;
    has Int $.powerRange    is rw;
    method damage () { random($.power - $.powerRange, $.power + $.powerRange);};
}

class Room is wObject { 
   has Monster @.monsters is rw;
   has Str     @.exits is rw;
   method are_monsters () { @.monsters // 0 }
   method monster ()      {
      say '@.monsters : ', @.monsters.perl if $DEBUG;
      my $x = shift @.monsters;
      say 'shifted    : ', $x.perl if $DEBUG;
      say '@.monsters : ', @.monsters.perl if $DEBUG;
      $x;
    }
};

class Mortal is wObject {
    has Int     $.life      is rw;
    has Int     $.max_life  is rw;
    
    has Weapon  $.weapon    is rw;
    method damage ($damage) {
           $.life -= $damage;
           $.life = 0 if $.life < 0;
    }
    
    method hit  (Mortal $enemy) {
      my $weapon = $.weapon;
      my $power  = $.weapon.damage;
      if ($power > 0) {
            say "$.name attacks $enemy.name() ",
                "with $weapon.name() doing $power damage!";
            $enemy.damage($power);                
      } elsif ($power < 0) {
            say "$.name hurts himself doing $power damage!";
            ./damage($power);                
      }       
    }
    method dead ()  { $.life <= 0 }    
}

class Person is Mortal {
    has Weapon  @.weapons   is rw;

    method battle (Mortal $enemy) {
        my $choice;

        say '';
        say "$enemy.name() is attacking you! What will you do?";

        until ($choice eq 'f' or $enemy.dead) {
            my @options;
            for @.weapons -> $wep {
                push @options, Option.new(
                     :text("attack with $wep.name()"),
                     :param($wep)
                );
            }
            push @options , Option.new( :key<f>, :text("flee for your life"));
            $choice = prompt("Your choice? ", @options);
            cls;
            given $choice {
                when 'f' {
                    say "You ran away from the $enemy.name()!"; 
                }
                #when Weapon {  #not yet working right.
                when .does(Weapon) {
                    $.weapon = $_;
                    ./attack($enemy);
                } 
                default {
                    say "Please enter a valid command!"
                }
            }
      }
      unless ($choice eq 'f') {
        say "The $enemy.name() is dead!";
        return 1;
      }
      return 0;
    }
      
    method attack (Monster $enemy) {
        ./hit($enemy);
        $enemy.hit($_);

        say '';
        say "Your health: $.life/$.max_life\t",
            "$enemy.name(): $enemy.life()/$enemy.max_life()";

        exit if .dead;
    }

    
}

class Monster is Mortal { }

my $person = Person.new(:life(100),:max_life(100),
	:weapons((Weapon.new(:name<sword>, :power(4), :powerRange(2)),
             Weapon.new(:name<spell>, :power(0), :powerRange(7)))),
);


my $frogs  = sub {
    my $life = 10 .?. 20;
    Monster.new(:name("Army of frogs"), :gold(0 .?. 100), :life($life),:max_life($life),
               :weapon(Weapon.new(:name<froggers>, :power(5), :powerRange(2))) );
};
my $bat    = sub {
    my $life = 20 .?. 30;
    Monster.new(:name("Bat"), :gold(0 .?. 100), :life($life), :max_life($life),
               :weapon(Weapon.new(:name<claws>, :power(5), :powerRange(3))) );
};
my $skeleton  = sub {
    my $life = 30 .?. 50;
    Monster.new(:name("Skeleton"), :gold(0 .?. 100), :life($life),:max_life($life),
               :weapon(Weapon.new(:name<Fists>, :power(5), :powerRange(10))) );
};
my %world;
%world<Lobby>   = Room.new( :name("Lobby")  , :exits("Forest","Dungeon"), :monsters([$frogs()]));
%world<Forest>  = Room.new( :name("Forest") , :exits("Lobby"), :monsters([$bat()]));
%world<Dungeon> = Room.new( :name("Dungeon"), :exits("Lobby"), :monsters([$skeleton()]));
$person.last_location = $person.location = "Lobby";

$person.name = capitalize(prompt("What is your name: "));
say "Greetings, $person.name()!";
say $person.where;
until ($person.dead) {
  %world.{$person.location}.perl.say if $DEBUG;
  if (%world.{$person.location}.are_monsters){ 
     my $monster = %world.{$person.location}.monster;
     unless ( $person.battle($monster) ) {
         push %world.{$person.location}.monsters, $monster;
         $person.location = $person.last_location;
     }
  } else {
     my @choices = %world.{$person.location}.exits.map:{ Option.new( :text($_), :param($_)) };
     $person.last_location = $person.location;
     $person.location = prompt("Go to:" ,@choices);
     cls;
  }
}
