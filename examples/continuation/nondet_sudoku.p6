#!/usr/bin/pugs

=head1 NAME

nondet_sudoku.p6 - A non deterministic sudoku solver

=head1 DESCRIPTION

A non-working perl 6 implementation of a non deterministic sudoku solver.  

Once we have full continuations, I'll revisit this and add a proper UI, right
now it's simply a demonstration of possibility.

=head1 AUTHOR

Piers Cawley E<lt>pdcawley@bofh.org.ukE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Piers Cawley. All rights reserved.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut

use v6;

sub callcc (Code &block) { &block(&?CALLER_CONTINUATION) }

my &give_up = sub { die "Program failed" };

sub choose (*@all_choices) {
  my &old_give_up = &give_up;
  callcc -> $cnt {
    my $try = -> @choices {
      if ! @choices {
        &give_up = &old_give_up;
        give_up;
      }
      else {
        my($choice, @newchoices) = *@choices;
        &give_up = sub { $cnt($try(@newchoices)) };
        $choice;
      }
    };
    $try(@all_choices);
  };
}

sub newchoose (*@all_choices) {
  my &old_give_up = &give_up;
  {
    my $try = -> @choices {
      if ! @choices { &give_up = &old_give_up; give_up }
      else {
        my ($choice, @newchoices) = *@choices;
        &give_up = -> { return $try(@newchoices) };
        $choice;
      }
    };
    $try(@all_choices);
  };
}

# Solve a 4x4 sudoku

my @grid = ( [ choose(1..4), choose(1..4), 2, choose(1..4) ],
             [ 1, choose(1..4), choose(1..4), choose(1..4) ],
             [ choose(1..4), choose(1..4), choose(1..4), 3 ],
             [ choose(1..4), 4, choose(1..4), choose(1..4) ] );

# Row assertions
give_up unless @grid[0].uniq == 4;
give_up unless @grid[1].uniq == 4;
give_up unless @grid[2].uniq == 4;
give_up unless @grid[3].uniq == 4;

# Column assertions
give_up unless [map -> $a {$a[0]}, @grid[0..3]].uniq == 4;
give_up unless [map -> $a {$a[1]}, @grid[0..3]].uniq == 4;
give_up unless [map -> $a {$a[2]}, @grid[0..3]].uniq == 4;
give_up unless [map -> $a {$a[3]}, @grid[0..3]].uniq == 4;

# Subgrid assertions
give_up unless [@grid[0][0], @grid[0][1], @grid[1][0], @grid[1][1]].uniq == 4;
give_up unless [@grid[0][2], @grid[0][3], @grid[1][2], @grid[1][3]].uniq == 4;
give_up unless [@grid[2][0], @grid[2][1], @grid[3][0], @grid[3][1]].uniq == 4;
give_up unless [@grid[2][2], @grid[2][3], @grid[3][2], @grid[3][3]].uniq == 4;

for @grid -> @row { say @row };



