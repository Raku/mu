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

my @words;         # the database of words/names
my $current_word;  # the current word/name
my @letters;       # the letters in that word/name
my @solution;      # the ever-evolving solution
my @guesses;       # the current set of guesses by the user

my $number_of_tries = 0;  # number of bad guesses
my $allowed_tries   = 6;  # number of allowed bad guesses

## do our functions

sub cls returns Void {
    system( ($?OS eq any<MSWin32 mingw cygwin>) ?? 'cls' :: 'clear');
}

sub get_words returns Array {
    my $dict = open("hangman.dic") err die "Couldn't open 'hangman.dic'";
    for (=$dict) -> $_name {
        my $name = $_name; # << damn immutable variables
        chomp($name);
        @words.push($name);
    }
    $dict.close();
}

sub pick_word returns Str { any(@words).pick }

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
    ($number_of_tries >= $num) ?? $char :: ' ';
}

sub draw_hangman returns Str {
    join("\n", (
"Hangman (with the Pugs AUTHORS list)",
"",
"  +-----+       The commiter's name is:", # $current_word
"  |     |       { draw_board }",
"  |     { draw_if_greater_than('O', 1) }   ",
"  |    {
          draw_if_greater_than('/', 2) ~
          draw_if_greater_than('|', 3) ~
          draw_if_greater_than('\\', 4)
        }      You have already guessed:",
"  |    { draw_if_greater_than('/', 5) } {
          draw_if_greater_than('\\', 6)
        }      [@guesses[]]",
"  |         ",
"|-+--------|",
""));
}

## main loop

get_words();
$current_word = pick_word();

@letters = split("", $current_word);
@solution = ('' xx +@letters);

cls;

print draw_hangman(); 
print "\nguess a letter? ";   
my $letter;
while ($letter = =$*IN) {
    cls;
    chomp($letter);
    if (guess($letter)) {
        if (has_won()) {
            print draw_hangman();         
            print "\nYou won!!!!\n";
            exit();
        }
    }
    else {  
        $number_of_tries++;
        if ($number_of_tries >= $allowed_tries) {
            print draw_hangman(); 
            print "\nYou have exceedded the maximum number of tries.\nSorry, the commiter was '$current_word'\n";
            exit();
        }
    }    
    print draw_hangman();  
    print "\nguess a letter? ";  
}
