#!/usr/bin/pugs

use v6;

sub print_board (Array @b) returns Void {
    say "+---+---+---+";
    for (@b) -> $x, $y, $z {
        say "| $x | $y | $z |
+---+---+---+";
    }
}

my @g = ('.') xx 9;

print_board( @g );

my %player = ('X' => 'Player 1', 'O' => 'Player 2');
my $entered = any();
my $choice = any(0 .. 8);

my $player = 'X';
while (any(@g) eq '.') {
    
    say %player{$player} ~ " Enter the Position [1-9]:";
    my $idx = =$IN - 1;

    unless $idx == $choice {
        say "Please enter a value within 1-9";   
    }
    else {  
        if $idx == $entered {
            say "Element already entered at { $idx + 1 }";
        }
        else {
            @g[$idx] = $player;
            $entered = any($entered.values, $idx); # rebuild the junction            

            for (
                [ 0, 1, 2 ], [ 3, 4, 5 ], 
                [ 6, 7, 8 ], [ 0, 3, 6 ], 
                [ 1, 4, 7 ], [ 2, 5, 8 ], 
                [ 0, 4, 8 ], [ 2, 4, 6 ]
            ) -> $c {
                if @g[$c].join("") eq ('XXX' | 'OOO') {
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

