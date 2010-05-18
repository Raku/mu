use v6;
use File::Spec::Unix;

## declare global variables (globals RULE dude!)

my @letters;           # the letters in that committer's name
my @solution;          # the ever-evolving solution
my @guesses;           # the current set of guesses by the user

my $number_of_bad_guesses = 0;  # number of bad guesses
my $allowed_bad_guesses   = 6;  # number of allowed bad guesses

## do our functions

sub cls returns Sink {
    run(($?OS eq any <MSWin32 mingw cygwin>) ?? 'cls' !! 'clear');
}

sub get_committer_list (Str $dict_file) returns List {
    my @committers;
    my $dict = open($dict_file) or die "Couldn't open the AUTHORS file.\n"; 

    # Skip the intro text
    1 while $dict.lines ~~ /\S/;

    for $dict.lines -> $name {
        # Capture the real name part
        if $name ~~ /(<alpha>+)»\s*/ {
            my $realname = $0;
            # Remove nickname
            $realname ~~ s/\s*\".*\"\s*/ /;
            @committers.push($realname);
        }
    }
    $dict.close();
    return @committers;
}

sub pick_committer (@committers) returns Str {
    @committers.pick;
}

sub draw_board returns Str {
    my $output = '';
    my @letters if !defined(@letters);
#    for (0...+@letters-1) -> $i {
#        if @letters[$i] ~~ /[-.,\s]/ {
#            $output ~= @letters[$i];
#            @solution[$i] = @letters[$i];
#        }
#        elsif @solution[$i] ne '' {
#            $output ~= @solution[$i];
#        }
#        else {
#            $output ~= '_';
#        }
#    }
    for 0..+@letters-1 -> $item {
       if $item ~~ /<[\-\.,\s]>/ {
          $output = @letters[$item];
       } elsif @solution[$item] ne '' {
          $output = @solution[$item];
       } else {
          $output = '_';
       }
   } 
   return $output;
}

sub has_won returns Bool {
    @letters == @solution.grep:{ $_ ne '' };
}

sub guess (Str $g) returns Bool {
    my $guess = uc $g;
    return 1 if $guess eq any(@guesses);
    return 1 if $guess.chars > 1;
    @guesses.push($guess);
    my $success = 0;
    my $i;
    for 0 .. +@letters - 1 -> $i {
        if uc(@letters[$i]) eq $guess {
            @solution[$i] = @letters[$i];
            $success = 1;
        }
    }
    return $success;
}

sub draw_if_greater_than (Str $char, Int $num) returns Bool {
    ($number_of_bad_guesses >= $num) ?? $char !! ' ';
}

sub draw_hangman (Str $msg?) returns Str {
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
#use lib 'ext/File-Spec/lib', '../ext/File-Spec/lib', '../../ext/File-Spec/lib';
#use File::Spec;

my $progdir    = splitpath($*PROGRAM_NAME)[1] || ".";
my $dict       = canonpath("$progdir/../../AUTHORS");
say "Getting committers list...";
my @committers = get_committer_list($dict);
say "I got committers list..." ~ @committers>>.Str.join(",").perl.say;
my $current_committer = pick_committer(@committers);

@letters = split("", $current_committer);
@solution = ('' xx +@letters);

cls;
print draw_hangman("guess a letter? ");
while my $letter = $*IN.get {
    cls;

    if guess($letter) {
        if has_won() {
            print draw_hangman("You won!!!!\n");
            exit;
        }
    }
    else {
        $number_of_bad_guesses++;
        if $number_of_bad_guesses >= $allowed_bad_guesses {
            print draw_hangman(
                "You have exceeded the maximum number of tries.\n" ~
                "Sorry, the committer was '$current_committer'\n"
            );
            exit;
        }
    }

    print draw_hangman("guess a letter? ");
}


=begin pod
=head1 NAME

hangman.pl - Hangman (with the Pugs AUTHORS list)

=head1 DESCRIPTION

This is a perl6 implementation of the classic Hangman game
using the Pugs AUTHORS file as a word list.

=head1 AUTHORS

stevan little, E<lt>stevan@iinteractive.comE<gt>

Audrey Tang E<lt>autrijus@autrijus.orgE<gt>

Ingo Blechschmidt

James Mastros

Mark McConnell

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=end pod


## vim: ft=perl6
