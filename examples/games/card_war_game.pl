use v6-alpha;

=head1 NAME

    card_war_game.pl

=head1 DESCRIPTION

This program simulates the classic Card War Game as described on
I<http://en.wikipedia.org/wiki/War_(card_game)>.

It was written to test, to show and explain some cool Perl 6 features:

=over 12

=item cartesian 'X' operator

=item default value '//' operator

=item conditional '??/!!' operator

=item Array.uniq method

=item defining your own operators

=item subroutine parameters (including mapping of hashes to definitions)

=item given/when syntax

=item gather/take syntax

=back


=head1 USAGE

    $ pugs card_war_game.pl
    
    Player1 draws Ace of Clubs
    Player2 draws Jack of Hearts
    Player1 won the round
    Player1 takes back his Ace of Clubs
    Player1 won Jack of Hearts
    
    ... # few minutes later
    
    Player1 draws Queen of Diamonds
    Player2 draws Queen of Clubs
    War !!
    Player1 draws face down card
    Player2 draws face down card
    Player1 draws face down card
    Player2 draws face down card
    Player1 draws face down card
    Player2 draws face down card
    Player1 draws 9 of Clubs
    Player2 draws 7 of Clubs
    Player1 won the round
    Player1 takes back his Queen of Diamonds
    Player1 takes back his 10 of Clubs
    Player1 takes back his 3 of Hearts
    Player1 takes back his 10 of Diamonds
    Player1 takes back his 9 of Clubs
    Player1 won Queen of Clubs
    Player1 won Jack of Clubs
    Player1 won 9 of Spades
    Player1 won 2 of Spades
    Player1 won 7 of Clubs
    
    Player1 draws 5 of Clubs
    Player2 draws 4 of Hearts
    Player1 won the round
    Player1 takes back his 5 of Clubs
    Player1 won 4 of Hearts
    
    Player1 draws 2 of Hearts
    Player2 has ran out of cards
    
    RESULT: Player1 won the game

=head1 AUTHORS

bbkr (Pawel Pabian) L<cpan@bbkr.org>

=cut

# construct card deck
# Perl 6 'X' opeator is used to create cartesian product of values and colors
# so @deck = ( [2, 'Hearts'], [2, 'Clubs'], [2, 'Spades'], [2, 'Diamonds'], [3, 'Hearts'], ... );
my @deck = (2..10, <Jack Queen King Ace> X <Hearts Clubs Spades Diamonds>).pick(*);

# create players and split the deck between them (each player gets 26 cards to hand)
my $player1 = {'name' => 'Player1', 'hand' => [splice(@deck, 0, 26)], 'stack' => [] };
my $player2 = {'name' => 'Player2', 'hand' => [splice(@deck, 0, 26)], 'stack' => [] };

# main program loop
# game continues while both players can put cards
# from their hands to their stacks
while draw_cards(visible => 1) {

    # return winner of the round
    my $round_winner = compare_cards();

    if not defined $round_winner {
        # if both players draw cards of the same value we have 'War'
        say 'War !!';
        # every player puts 3 face-down cards from his hand to his stack
        draw_cards(visible => 0) or last for 1..3;
    }
    else {
        # round winner was defined
        say $round_winner<name>, ' won the round';
        # round winner gets both stacks
        collect_cards($round_winner);
        say;
    }
}

# one or both players ran out of cards
# Perl 6 chained comparison operators are used
print "\nRESULT: ";
if ($player1<hand>.elems == $player2<hand>.elems == 0) {
    say 'both players ran out of cards';
}
elsif ($player1<hand>.elems > $player2<hand>.elems == 0) {
    say $player1<name>, ' won the game';
}
elsif (0 == $player1<hand>.elems < $player2<hand>.elems) {
    say $player2<name>, ' won the game';
}

exit 0;

# Perl 6 creating new operators
# define CC (compare cards) operator
sub infix:<CC> (ArrayRef $card1, ArrayRef $card2) {
    my %values = (
        'Jack'   => 11,
        'Queen'  => 12,
        'King'   => 13,
        'Ace'    => 14,
    );
    # Perl 6 default values operator '//'
    # if card value was not numeric, for example 'Ace', get mapped value
    # if not mapped value was defined just get card value (it was numeric)
    my $value1 = %values{$card1[0]} // $card1[0];
    my $value2 = %values{$card2[0]} // $card2[0];

    return $value1 <=> $value2;
}

# use CC operator to compare top cards on player stacks.
# return player that won
sub compare_cards {
    my $compare = $player1<stack>[-1] CC $player2<stack>[-1];

    # Perl 6 'given/when' operator (works like switch)
    given $compare {
        when 1 { return $player1 }
        when -1 { return $player2 }
        when 0 { return }
    }
}

# both players pops one card from their hands to their stacks
# returns true if both players could pop cards
sub draw_cards (Bool $visible) {
    # Perl 6 'gather/take' syntax used to check
    # if both players had cards to pop
    my @cards = gather for $player1, $player2 -> $player {
        my $card = $player<hand>.pop;
        if defined $card {
            # Perl 6 '?? !!' operator (replacement for '?:')
            say $player<name>, ' draws ', ($visible ?? $card.[0] ~ ' of ' ~$card.[1] !! 'face down card');
            # push popped card to players stack
            $player<stack>.push( $card );
            take $card;
        }
        else {
            say $player<name>, ' has ran out of cards';
        }
    };

    # return True if both players had cards to pop
    return @cards.elems == 2;
}

# player that won the round gets both stacks to his hand
sub collect_cards(HashRef $winner) {
    # Perl 6 uniq method is used to get array
    # containing winner and the other player
    for @($winner, $player1, $player2).uniq -> $player {
        for @($player<stack>) -> $card {
            # Perl 6 smart match operator to compare HashRefs
            if $player ~~ $winner {
                say $winner<name>, ' takes back his ', $card[0], ' of ', $card[1];
            }
            else {
                say $winner<name>, ' won ', $card[0], ' of ', $card[1];
            }
            $winner<hand>.unshift( $card );
        }
        # clear stacks
        $player<stack> = [];
    }
}

__END__
