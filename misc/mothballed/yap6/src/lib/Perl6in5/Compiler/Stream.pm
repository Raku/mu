#
# This software is Copyright 2005 by Elsevier Inc.  You may use it
# under the terms of the license at http://perl.plover.com/hop/LICENSE.txt .
#



###
### Stream.pm
###

## Chapter 6 section 2

package Perl6in5::Compiler::Stream;
use base Exporter;
@EXPORT_OK = qw(node head tail drop upto upfrom show promise
                filter transform merge list_to_stream cutsort
                iterate_function cut_loops);

%EXPORT_TAGS = ('all' => \@EXPORT_OK);

sub node {
  my ($h, $t) = @_;
  [$h, $t];
}

sub head {
  my ($s) = @_;
  $s->[0];
}

sub tail {
  my ($s) = @_;
  if (is_promise($s->[1])) {
    return $s->[1]->();
  }
  $s->[1];
}

sub is_promise {
  UNIVERSAL::isa($_[0], 'CODE');
}
sub promise (&) { $_[0] }


## Chapter 6 section 2.1

sub upto {
  my ($m, $n) = @_;
  return if $m > $n;
  node($m, promise { upto($m+1, $n) } );
}
sub upfrom {
  my ($m) = @_;
  node($m, promise { upfrom($m+1) } );
}


## Chapter 6 section 2.2

sub show {
  my ($s, $n) = @_;
  while ($s && (! defined $n || $n-- > 0)) {
    print head($s), $";
    $s = tail($s);
  }
  print $/;
}


## Chapter 6 section 2.2

sub drop {
  my $h = head($_[0]);
  $_[0] = tail($_[0]);
  return $h;
}


## Chapter 6 section 2.2

sub transform (&$) {
  my $f = shift;
  my $s = shift;
  return unless $s;
  node($f->(head($s)),
       promise { transform($f, tail($s)) });
}


## Chapter 6 section 2.2

sub filter (&$) {
  my $f = shift;
  my $s = shift;
  until (! $s || $f->(head($s))) {
    drop($s);
  }
  return if ! $s;
  node(head($s),
       promise { filter($f, tail($s)) });
}


## Chapter 6 section 3.1

sub tail {
  my ($s) = @_;
  if (is_promise($s->[1])) {
    $s->[1] = $s->[1]->();
  }
  $s->[1];
}


## Chapter 6 section 4

sub merge {
  my ($S, $T) = @_;
  return $T unless $S;
  return $S unless $T;
  my ($s, $t) = (head($S), head($T));
  if ($s > $t) {
     node($t, promise {merge(     $S,  tail($T))});
   } elsif ($s < $t) {
     node($s, promise {merge(tail($S),      $T)});
   } else {
     node($s, promise {merge(tail($S), tail($T))});
   }
}


## Chapter 6 section 5.3

sub list_to_stream {
  my $node = pop;
  while (@_) {
    $node = node(pop, $node);
  }
  $node;
}
sub insert (\@$$);

sub cutsort {
  my ($s, $cmp, $cut, @pending) = @_;
  my @emit;

  while ($s) {
    while (@pending && $cut->($pending[0], head($s))) {
      push @emit, shift @pending;
    }

    if (@emit) {
      return list_to_stream(@emit, 
                            promise { cutsort($s, $cmp, $cut, @pending) });
    } else {
      insert(@pending, head($s), $cmp);
      $s = tail($s);
    }
  }

  return list_to_stream(@pending, undef);
}


## Chapter 6 section 5.3

sub insert (\@$$) {
  my ($a, $e, $cmp) = @_;
  my ($lo, $hi) = (0, scalar(@$a));
  while ($lo < $hi) {
    my $med = int(($lo + $hi) / 2);
    my $d = $cmp->($a->[$med], $e);
    if ($d <= 0) {
      $lo = $med+1;
    } else {
      $hi = $med;
    }
  }
  splice(@$a, $lo, 0, $e);
}


## Chapter 6 section 6.1

sub iterate_function {
  my ($f, $x) = @_;
  my $s;         
  $s = node($x, promise { &transform($f, $s) });
}


## Chapter 6 section 6.3

sub cut_loops {
  my ($tortoise, $hare) = @_;
  return unless $tortoise;

  # The hare and tortoise start at the same place
  $hare = $tortoise unless defined $hare;

  # The hare moves two steps every time the tortoise moves one
  $hare = tail(tail($hare));

  # If the hare and the tortoise are in the same place, cut the loop
  return if head($tortoise) == head($hare);

  return node(head($tortoise), 
              promise { cut_loops(tail($tortoise), $hare) });
}


## Chapter 6 section 6.3

sub cut_loops2 {
  my ($tortoise, $hare, $n) = @_;
  return unless $tortoise;
  $hare = $tortoise unless defined $hare;

  $hare = tail(tail($hare));
  return if head($tortoise) == head($hare)
            && $n++;
  return node(head($tortoise), 
              promise { cut_loops(tail($tortoise), $hare, $n) });
}

1;