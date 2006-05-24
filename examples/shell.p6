#!/usr/bin/pugs
use v6;

# A simple shell written in Perl6

# TODO
# make it work...
# BACKPSACE,
# editing ?


my $prompt = '<p6shell>$';
my $VERSION = '0.01';

# Enable reading character as they ar typed, see Perl5: perldoc -f getc  
# It would be better to use Term::ReadKey but it has to be implemented for Perl6
my $BSD_STYLE = 1;

if ($BSD_STYLE) {
    system "stty cbreak </dev/tty >/dev/tty 2>&1";
}
else {
    system "stty", '-icanon', 'eol', "\001";
}

loopit();

if ($BSD_STYLE) {
    system "stty -cbreak </dev/tty >/dev/tty 2>&1";
}
else {
    system "stty", 'icanon', 'eol', '^@'; # ASCII null
}
exit;

####################################################33

sub loopit {
    while(1) {
        my $command = '';
        print "\n", $prompt;
        while (1) {
            my $char = $*IN.getc;
            if ($char eq "\n") {
                eval $command;
                last;
            }
            if ($char eq "\t") {
                # clean entry line
                print "\r", $prompt;
                print " " x $command.bytes + 1;
                print "\r", $prompt;
                $command = tab_completition($command);
                print $command;
                next;
            }
            $command ~= $char;
        }
        if ($command eq "exit") {
            last;
        }
    }
}


# should understand the line so far....
sub tab_completition {
    my ($command) = @_;

    my @available_commands = <exit print>;
    my @possible_commands = grep { not index($_, $command)}, @available_commands;
    # might really get more than one... and we should let the user step through them using TAB
    if (@possible_commands) {
        return @possible_commands[0];
    }

    return $command;
}


