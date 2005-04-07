#!/usr/bin/pugs

use v6;

## ------------------------------------------------------------------
## Hangman (with the Pugs AUTHORS list)
## ------------------------------------------------------------------
## The following is a example of what perl6 code might have looked 
## like if it was around in 1996 :)
##
## Actually this is a rough cut for a CGI based hangman game, I 
## thought that I would try it without the web component first, 
## and then port it to CGI. 
## ------------------------------------------------------------------

## declare global variables (globals RULE dude!)

my @letters;           # the letters in that committer's name
my @solution;          # the ever-evolving solution
my @guesses;           # the current set of guesses by the user

my $number_of_bad_guesses = 0;  # number of bad guesses
my $allowed_bad_guesses   = 6;  # number of allowed bad guesses

## do our functions

sub cls returns Void {
    system(($?OS eq any<MSWin32 mingw cygwin>) ?? 'cls' :: 'clear');
}

sub get_committer_list (Str $dict_file) returns List {
    my @committers;
    my $dict = open($dict_file) err die "Couldn't open '$dict_file'";

    # Skip the intro text
    1 while =$dict ~~ rx:perl5/\S/;

    for (=$dict) -> $name {
        # Capture the real name part
        if ($name ~~ rx:perl5/^(.+?)(?:\s\s|$)/) {
            my $realname = $1;
            # Remove nickname
            $realname ~~ s:perl5/\s*".*"\s*/ /;
            @committers.push($realname);
        }
    }
    $dict.close();
    return @committers;
}

sub pick_committer (@committers) returns Str {
    any(@committers).pick;
}

sub draw_board returns Str { 
    my $output = '';
    for (0 .. (+@letters - 1)) -> $i {
        if (@letters[$i] ~~ rx:perl5{[-.,\s]}) {
            $output ~= @letters[$i];
            @solution[$i] = @letters[$i];
        }
        elsif (@solution[$i] ne '') {
            $output ~= @solution[$i];
        }
        else {
            $output ~= '_';
        }
    }
    return $output;
}

sub has_won returns Bool {
    @letters == @solution.grep:{ $_ ne '' };
}

sub guess (Str $guess) returns Bool {
    return 1 if $guess eq any(@guesses);
    @guesses.push($guess);
    my $success = 0;
    my $i;
    loop ($i = 0; $i < +@letters; $i++) {
        if (lc(@letters[$i]) eq lc($guess)) {
            @solution[$i] = @letters[$i];
            $success = 1;
        }
    }
    return $success;
}

sub draw_if_greater_than (Str $char, Int $num) returns Bool { 
    ($number_of_bad_guesses >= $num) ?? $char :: ' ';
}

sub draw_hangman (Str ?$msg) returns Str {
    "Hangman (with the Pugs AUTHORS list)
    
  +-----+       The committer's name is:
  |     |       { draw_board }
  |     { draw_if_greater_than('O', 1) }   
  |    {
          draw_if_greater_than('/', 2) ~
          draw_if_greater_than('|', 3) ~
          draw_if_greater_than('\\', 4)
       }      You have already guessed:
  |    { draw_if_greater_than('/', 5) } {
          draw_if_greater_than('\\', 6)
       }      [@guesses[]]
  |         
|-+--------|

$msg";
}

## main loop
unshift @*INC, 'ext/FileSpec/lib', '../../ext/FileSpec/lib';
require File::Spec;
my ($progdir) = splitpath($*PROGRAM_NAME)[1];
my $dict = canonpath("$progdir../../AUTHORS");
my @committers = get_committer_list($dict);
my $current_committer = pick_committer(@committers);

@letters = split("", $current_committer);
@solution = ('' xx +@letters);

cls;
print draw_hangman("guess a letter? "); 
my $letter;
while ($letter = =$*IN) {
    cls;
    chomp($letter);

    if (guess($letter)) {
        if (has_won()) {
            print draw_hangman("You won!!!!\n");         
            exit;
        }
    }
    else {
        $number_of_bad_guesses++;
        if ($number_of_bad_guesses >= $allowed_bad_guesses) {
            print draw_hangman(
                "You have exceedded the maximum number of tries.\n" ~
                "Sorry, the committer was '$current_committer'\n"
            ); 
            exit;
        }
    }

    print draw_hangman("guess a letter? ");  
}
