use v6;

multi sub prompt (Str ?$prompt) {
    print "$prompt";
    my $temp; ($temp= =<>).chomp;
    say "";
    return $temp;
}

multi sub prompt ($prompt, @data is copy) {
    my $i = 1;
    for @data -> $item { $item[0] //= $i++; };
 	 my $choice;
    while (not @data.grep:{ $_[0] eq $choice} ) {
    	say $prompt;
      for @data -> $item {
      	say "\t", $item[0], " ", $item[1];
      }
      $choice = prompt;
    }
    for @data -> $item { return $item[2] // $item[0] if $item[0] eq $choice;}
    return $choice;
}	

sub cls { system(($?OS eq any<MSWin32 mingw>) ?? 'cls' :: 'clear'); }
multi sub infix:<<.?.>> ($low,$high) {int ( rand ($high - $low) + $low ) + 1; };

class Object {
	has Str $.name     is rw;
   has Str $.location is rw;
   has Int $.plural;
   method where () {
      return $.name ~ ($.plural ?? " are" :: " is") ~
      		 " currently in the $.location";
   };
};
   
class Weapon is Object {
    has Int $.power         is rw;
    has Int $.powerRange    is rw;
    method damage () { $.power - $.powerRange .?. $.power + $.powerRange;};
}

class Room is Object { 
	has Monster @.monsters is rw;
   has Str     @.exits is rw;
   method are_monsters () { @.monsters // 0 }
   method monster ()      { shift @.monsters; }
};

class Mortal is Object {
    has Int     $.life      is rw;
    has Weapon  $.weapon    is rw;
    method damage ($damage) {
           $.life -= $damage;
           $.life = 0 if $.life < 0;
    }
    
    method hit  (Mortal $enemy) {
      my $weapon = $.weapon;
      my $power  = $.weapon.damage;
      if ($power > 0) {
            say $.name ~ " attacks $enemy.name() " ~
                "with $weapon.name() doing $power damage!";
            $enemy.damage($power);                
      } elsif ($power < 0 ) {
            say $.name ~ " hurts himself doing $power damage!";
            ./damage($power);                
      }       
    }
    method dead ()  { $.life <= 0 }    
}

class Person is Mortal {
    has Weapon  @.weapons   is rw;

    method battle (Mortal $enemy) {
        my $choice;

        say "\n", $enemy.name, " is attacking you! What will you do?";
        until ($choice eq 'f' or $enemy.dead) {
        		my @choices;
            for @.weapons -> $wep {
                push @choices , [undef,"attack with $wep.name()", $wep];
            }
            push @choices , ['f', "flee for your life",undef];
            $choice = prompt("Your choice?",@choices);
            cls;            
            given $choice {
                when 'f' {
                    say "You ran away from the $enemy.name()!"; 
                }
                if ( @.weapons.exists($_) ) {
                    $.weapon = @.weapons[$_];
                    ./attack($enemy);
                } else {
                    say "Please enter a valid command!"
                }
            }
        }
        say "The $enemy.name() is dead!" unless $choice eq 'f';
    }
      
    method attack (Monster $enemy) {
        ./hit($enemy);
        $enemy.hit($_);
        say "";
        say "Your health: $.life\t$enemy.name(): $enemy.life()";
        exit if .dead;
    }

    
}

class Monster is Mortal { }

my $person = Person.new(:life(100),
	:weapons((Weapon.new(:name<sword>, :power(4), :powerRange(2)),
   			 Weapon.new(:name<spell>, :power(0), :powerRange(7)))),
);


my $frogs  = Monster.new(:name("Army of frogs"), :gold(int rand 100), :life((20..30).pick),
                         :weapon(Weapon.new(:name<froggers>, :power(5), :powerRange(2))) );
my $bat    = Monster.new(:name("Bat"), :gold(int rand 100), :life((10..30).pick),
                         :weapon(Weapon.new(:name<claws>, :power(6), :powerRange(2))) );
my $skeleton  = Monster.new(:name("Skeleton"), :gold(int rand 100), :life((40..50).pick),
                         :weapon(Weapon.new(:name<Fists>, :power(10), :powerRange(10))) );
                         
my %world;
%world<Lobby>   = Room.new( :name("Lobby")  , :exits("Forest","Dungeon"), :monsters([$frogs]));
%world<Forest>  = Room.new( :name("Forest") , :exits("Lobby"), :monsters([$bat]));
%world<Dungeon> = Room.new( :name("Dungeon"), :exits("Lobby"), :monsters([$skeleton]));
$person.location = "Lobby";
cls;
$person.name = capitalize(prompt("What is your name:"));
say "Greetings, $person.name()!";
say $person.where;
until ($person.dead) {
  if (%world.{$person.location}.are_monsters > 0){ 
  	  my $monster = shift %world.{$person.location}.monster;
     $person.battle($monster);
  } else {
  	  my @choices = %world.{$person.location}.exits.map:{ [undef, $_,$_] };
     $person.location = prompt("Go to:" ,@choices);
  }
}
