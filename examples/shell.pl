use v6-alpha;

# A simple shell written in Perl6

# TODO
# BACKPSACE, history, editing ?


my $prompt = '<p6shell>$ ';
my $VERSION = '0.01';

# we should have this list from some internal command
# probably along with the signature of these functions
my @available_commands = <exit print say>;
@available_commands.push( <mkdir rmdir chdir unlink chmod chown> );
@available_commands.push( <pop push> );


# Enable reading character as they ar typed, see Perl5: perldoc -f getc  
# It would be better to use Term::ReadKey but it has to be implemented for Perl6
my $BSD_STYLE = 1;

if ($BSD_STYLE) {
    system "stty cbreak </dev/tty >/dev/tty 2>&1";
}
else {
    system "stty", '-icanon', 'eol', "\001";
}

my $_loop_ = get_loop();
eval $_loop_;

if ($BSD_STYLE) {
    system "stty -cbreak </dev/tty >/dev/tty 2>&1";
}
else {
    system "stty", 'icanon', 'eol', '^@'; # ASCII null
}
exit;

#################################################333

sub get_loop {
return '
    while(1) {
        my $command = "";
        print "\n", $prompt;
        while (1) {
            my $char = $*IN.getc;
            if ($char eq "\n") {
                # TODO: maybe check if _loop_ shows up in the input and disallow that code ?
                if (eval "$command;" ~ $_loop_ ) {
                    exit;
                } 
                else {
                    print $!;
                    last;
                }
            }
            if ($char eq "\t") {
                # clean the TAB but keep what we had so far
                refresh_commandline($command);

                my $tail = tab_completition($command);

                if (defined $tail) {
                    $command ~= $tail;
                    refresh_commandline($command);
                }
                next;
            }
            $command ~= $char;
        }
    }
';
}

# TODO: this should understand the command line typed in so far....
sub tab_completition {
    my ($command) = @_;

    my @possible_commands = grep { not index($_, $command)}, @available_commands;
    # TODO: might really get more than one... and we should let the user step through them using TAB
    # or display all possible values, or the user should be able to configure the behivaior
    return if not @possible_commands;
    return substr(@possible_commands[0], $command.bytes) if 1 == @possible_commands;

    # TODO: if there are too many (> $LIMIT) ask if the user really wants to display all
    my $WIDTH = 80;
    my $out = '';
    my $line = '';
    for @possible_commands -> $com {
        if ($line.bytes + 1 + $com.bytes <= $WIDTH) {
            $line ~= " $com";
        } else {
            $out ~= "$line\n";
            $line = $com;
        }
    }
    $out ~= "$line\n";
    print "\n$out";
    return "";
}

sub refresh_commandline {
    my ($command) = @_;
    print "\r", $prompt;
    print " " x $command.bytes + 1;
    print "\r", $prompt;
    print $command;
}

