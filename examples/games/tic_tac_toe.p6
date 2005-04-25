#!/usr/bin/pugs

use v6;

sub print_board ( @b ) {
    my $count = 0;
    for (@b) -> $x {
        print "$x\t";
        print "\n" unless ++$count % 3;
    }
}

my @g = ('.') xx 9;

print_board( @g );

my %player = ('X','Player 1','O','Player 2');

my %entered;
my $choice = any (1 .. 9);

my $player = 'X';
while (grep { $_ eq '.' } @g) {
    
    say %player{$player} ~ " Enter the Position [1-9]:";
    my $in = =$IN;

    unless ($in == $choice) {
        say "Please enter a value within 1-9";   
    }
    else {
        my $idx = $in - 1;
        if (%entered.exists($idx)) {
            say "Element already entered at $in";
        }
        else {
            @g[$idx] = $player;
            %entered{$idx}++;

            for (
                [ 0, 1, 2 ], [ 3, 4, 5 ], 
                [ 6, 7, 8 ], [ 0, 3, 6 ], 
                [ 1, 4, 7 ], [ 2, 5, 8 ], 
                [ 0, 4, 8 ], [ 2, 4, 6 ]
            ) -> $c {
                if (@g[$c[0]] ne '.' && @g[$c[0]] eq @g[$c[1]] eq @g[$c[2]]) {
                    print_board( @g );
                    say "  " ~ %player{$player} ~ " Wins \n";
                    exit();
                }
            }

            print_board( @g );
            $player = $player eq 'X' ?? 'O' :: 'X';
        }
    }
}

=pod

=head1 NAME

tic_tac_toe.p6 - Tic-Tac-Toe

=head1 DESCRIPTION

This is a perl6 implementation of the classic Tic-Tac-Toe game.

=head1 AUTHORS

mkirank L<http://www.perlmonks.org/index.pl?node_id=451261>

rob kinyon L<http://www.perlmonks.org/index.pl?node_id=451302>

stevan little, E<lt>stevan@iinteractive.comE<gt> 

=cut

