#!/usr/bin/pugs

use v6;

my @words;
my @letters;
my @guesses;
my $word_junction;
my $current_word;

my $number_of_tries = 0;
my $allowed_tries   = 6;

sub get_words returns Array {
    my @w;
    my $dict = open("hangman.dic") err
      die "Couldn't open \"hangman.dic\"! Did you run $*PROGRAM_NAME from its directory?\n";
    for (=$dict) -> $_name {
        my $name = $_name;
        chomp($name);
        push(@w, $name);
    }
    $dict.close();
    return @w;
}

sub pick_word returns Str { $word_junction.pick() }

@words = get_words();
$word_junction = any(@words);

$current_word = pick_word();

@letters = split("", $current_word);
@guesses = ('' xx +@letters);

sub draw_board returns Str { 
    my $output = '';
    for (0 .. (+@letters - 1)) -> $i {
        if (@letters[$i] ~~ rx:perl5{\s|\-|\.}) {
            $output ~= @letters[$i];
            @guesses[$i] = @letters[$i];
        }
        elsif (@guesses[$i] ne '') {
            $output ~= @guesses[$i];
        }
        else {        
            $output ~= '_';
        }
    }
    return $output;
}

sub has_won returns Bool { +@letters == +(@guesses.grep:{ $_ ne '' }) }

sub guess (Str $guess) returns Bool {
    my $success = 0;
    my $i;
    loop ($i = 0; $i < +@letters; $i++) {
        if (lc(@letters[$i]) eq lc($guess)) {
            @guesses[$i] = @letters[$i];
            $success = 1;
        }
    }
    return $success;
}

sub draw_if_greater_than (Str $char, Int $num) returns Bool { (($number_of_tries >= $num) ?? $char :: ' ') }

sub draw_hangman returns Str {
join("\n", (
"Hangman (with the Pugs AUTHORS list)",
"",
"  +-----+       The commiter's name is:", # $current_word
"  |     |       " ~ draw_board(),
"  |     " ~ draw_if_greater_than("O", 1) ~ "   ",
"  |    "  ~ draw_if_greater_than("/", 2) ~ draw_if_greater_than("|", 3) ~ draw_if_greater_than("\\", 4) ~ "  ",
"  |    "  ~ draw_if_greater_than("/", 5) ~ " " ~ draw_if_greater_than("\\", 6) ~ "  ",
"  |         ",
"|-+--------|",
""));
}


#system('clear');
print draw_hangman(); 
print "\nguess a letter? ";   
my $letter;
while ($letter = =$*IN) {
    #system('clear');
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
            print "\nYou have exceedded the maximum number of tries. Sorry, the commiter was '$current_word'\n";
            exit();
        }
    }    
    print draw_hangman();  
    print "\nguess a letter? ";  
}
