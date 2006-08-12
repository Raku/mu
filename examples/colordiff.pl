#!/usr/bin/pugs
#
# Perl 6 variation of http://www.perlmonks.org/?node_id=567025.
# You can change $command to reflect the actual command you want to use
# (e.g. 'svk diff').

use v6;

# Default command, a hint on a possible different default is given as well
# If $default_command is undef, then the command line will be called as-is
my $default_command = 'diff';
 $default_command = 'svk diff';

# Color associations: red are "from", green are "to", blue are other stuff
my %color_for = (
    '<' => RED,
    '-' => RED,
    '>' => GREEN,
    '+' => GREEN,
    '@' => BLUE,
    '=' => BLUE,
);

# If there are arguments, call the default command, otherwise get stdin
my $fh = $*IN;
if (@*ARGS.elems) {
    my @args = @*ARGS;
    unshift @args, $default_command if defined $default_command;
    my $command = @args.map({ quotemeta($_) }).join(' ');
    $fh = get_input_fh($command);
}

# Iterate over input and color it
for =$fh {
    my $first_char = substr $_, 0, 1;
    delete %color_for{'-'} if $first_char eq '<';
    print BOLD, %color_for{$first_char} if %color_for.exists($first_char);
    say;
    print RESET if %color_for.exists($first_char);
}
close $fh;

# Stripped down version of Term::ANSIColor
sub _color ($color) { return "\x1b[" ~ $color ~ "m"; }
sub RED   { return _color(31); }
sub GREEN { return _color(32); }
sub BLUE  { return _color(34); }
sub BOLD  { return _color( 1); }
sub RESET { return _color( 0); }

######### WORKAROUNDS #####################################################

# Wrapper function to substitute Perl 5 idiom:
#
#    open my $fh, '-|', $command;
#
# Will do better in the future, using Prelude
sub get_input_fh ($command) {
    my ($in, $out, $err, $pid) =
        Pugs::Internals::runInteractiveCommand($command);
    return $out;
}

# I'm not using rules to be fair with who doesn't install parrot.
# Ok, I've not installed parrot :)
sub quotemeta ($string) {
    state $wordish = (0 .. 9) ~ ('a' .. 'z') ~ ('A' .. 'Z') ~ '_';
    state %wordish = $wordish.split('').map({ $_ => 1; });
    return $string.split('').map({
        %wordish.exists($_) ?? $_ !! "\\$_";
    }).join('');
}
