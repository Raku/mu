use v6;

sub print_board (@b) { # returns Void {
    say "+---+---+---+";
    for @b -> $x, $y, $z {
        say "| $x | $y | $z |";
        say "+---+---+---+";
    }
}

my %player  = ( X => 'Player 1', O => 'Player 2' );
my $entered = any();
my $choice  = one(0 .. 8);

my $player = 'X';
my @board   = (1..9);

print_board @board;

while (any(@board) eq one(1..9) ) {
    say %player{$player} ~ ": Enter the Position [1-9]:";

    my $idx = $*IN.get - 1;

    if (!($idx eq $choice)) {
        say "*** Please enter a value within 1-9";
    }
    elsif $idx eq $entered {
        say "*** Some one already played at { $idx + 1 }";
    }
    else {
        $entered |= $idx;
        @board[$idx] = $player;

        for (
            [ 0, 1, 2 ], [ 3, 4, 5 ],
            [ 6, 7, 8 ], [ 0, 3, 6 ],
            [ 1, 4, 7 ], [ 2, 5, 8 ],
            [ 0, 4, 8 ], [ 2, 4, 6 ]
        ) -> @c {
            if @board[@c] eq (<X X X>||<O O O>) {
                print_board @board;
                say "*** %player{$player} Wins!\n";
                exit;
            }
        }

        print_board @board;
        $player = ($player eq 'X') ?? 'O' !! 'X';
    }
}

=begin pod
=head1 NAME

tic_tac_toe.pl - Tic-Tac-Toe

=head1 DESCRIPTION

This is a perl6 implementation of the classic Tic-Tac-Toe game.

=head1 AUTHORS

mkirank L<http://www.perlmonks.org/index.pl?node_id=451261>

Rob Kinyon L<http://www.perlmonks.org/index.pl?node_id=451302>

Stevan Little, E<lt>stevan@iinteractive.comE<gt>

Audrey Tang

=end pod

