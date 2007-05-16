#!/usr/bin/perl

# Adventures in Perl6

use v6-alpha;

### GRAMMAR ###

grammar Adventure {
  token command  { 
	| <verb> <ws> <article> <ws> <object> 
	| <verb> <ws> <object>
	| <verb>
	| <direction>
  }

  token verb      { look|take|drop|inventory|open|score|help|quit }
  token object    { sign|coin|key|door|vampire|cross }
  token article   { a|an|the|at|in|on|to }
  token direction { north|south|east|west }
}


### GAME DATA ####

my %player = (
    place => "chamber",
    score => 0,
);

my %map = (
    chamber => {
        north => 'throne room',
        east  => 'dungeon',
    },
    'throne room' => {
        south => 'chamber',
    },
    dungeon => {
        west  => 'chamber',
        north => 'crypt',
        BLOCKED_south => 'cell', # there is no "south"... yet
    },
    crypt => {
        south => 'dungeon',
    },
    cell => {
        north => 'dungeon',
    },
);

my %object = (
    sign => {
        place => "chamber",
        description => "Sign says: bring treasures here, then say SCORE",
    },
    coin => {
        place => "cell",
    },
    key => {
        place => "crypt",
    },
    door => {
        place => "dungeon",
        description => "The door is closed",
    },
    vampire => {
        place => 'crypt',
    },
    cross => {
        place => 'throne room',
    }
);


### ACTIONS ###
module main;

sub help {
    say "look, take, drop, inventory, open, score, help, quit";
    say "north, south, east, west";
}

sub quit {
    score();
    exit;
}

sub walk_to( $direction ) {
    my $new_place = %map{%player{'place'}}{$direction};

    if $new_place { # can go to that direction?
        say "You entered the $new_place";
        %player{'place'} = $new_place;
    } else {
        say "You can't go $direction";
    }
}

sub look( $object ) {
  if i_see( $object ) {
      my $description =  %object{$object}{'description'};
      if $description {
          say $description;
      } else {
          say "It's a regular $object";
      }
  } else {
    # special command
    look_around();
  }
}

sub look_around {
  say "You are in the " ~ %player{'place'};

  my @objects = objects_in( %player{'place'} );
  my $list    = join(", ", @objects);

  say "I see here: $list" if $list;
}

sub inventory {
  my @objects = objects_in('player');
  my $list    = join(", ", @objects);

  $list = "nothing" if !$list;

  say "You're carrying: $list.";
}

sub take( $object ) {
  if is_here( $object ) {
    say "You took the $object";
    %object{$object}{'place'} = 'player';
  } else {
    say "I don't see that here";
  }
}

sub drop( $object ) {
  if i_have( $object ) {
    say "You dropped the $object on the floor";
    %object{$object}{'place'} = %player{'place'};
  } else {
    say "You don't have that";
  }
}

sub open( $object ) {
  if $object ne 'door' {
    say "You can't open that!";
    return;
  }

  if is_here( $object ) {
    if i_have('key') {
      say "You opened the door!";
      %object{'door'}{'description'}    = 'The door is open';
      %map{ %player{'place'} }{'south'} = 'cell';
    } else {
      say "The door is locked";
    }
  } else {
    say "I don't see a $object";
  }
}

sub score {
  if i_have('coin') {
    say "You got the treasure. Congratulations!";
  } else {
    say "You have scored " ~ %player{'score'} ~ " points";
  }
}


### AUXILIAR FUNCTIONS ###

sub is_here ($object) {
  return ( %object{$object}{'place'} eq %player{'place'} );
}

sub i_have ($object) {
  return ( %object{$object}{'place'} eq 'player' );
}

sub i_see ($object) {
  return ( is_here( $object) or i_have( $object ) );
}

sub objects_in ($where) {
  my @objects;
  for %object.keys -> $obj {
    push @objects, $obj if %object{$obj}{'place'} eq $where;
  }

  return @objects;
}


### MAIN LOOP ###

while (1) {
    print "> ";
    my $input = =<>;

    my $response = Adventure.command( $input )<command>;

    if $response{'direction'} {
        walk_to( ~$response{'direction'} );
    }
    elsif $response{'verb'} {
        my $command  = "main::" ~ $response{'verb'} ~ '("' ~ $response{'object'} ~ '")';
        eval($command);
        print $! if $!;
    } else {
        say "What?";
    }

    say;
}
