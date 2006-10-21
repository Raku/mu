use strict;

{
my $coro1;
my $coro2;
$coro2 = $coro1 = sub {
    my $x = shift;
    $coro1 = sub {
    my $coro2 = $coro1;
    $coro1 = sub { 
    $coro1 = $coro2;
    return 5;
    };
    return $x+1; 
    };
    return $x;
};

print $coro1->( 10 ), "\n";
print $coro1->(), "\n";
print $coro1->(), "\n";
}

{
my $coro1;
my $coro2;
$coro2 = $coro1 = sub {
    my $x = shift;
    my $end = 7;
    $coro1 = sub {
    $x++;
    my $coro2 = $coro1;
    $coro1 = sub { 
    $coro1 = $coro2;
    return;
    }
        unless $x <= $end;
    return $x if $x <= $end; 
    };
    return $x if $x <= $end;
};

  print $coro1->( 3 ), "\n";
  for ( 0 .. 4 ) {
    print $coro1->(), "\n";
  }
}

__END__

coro foo ($x) {
    yield $x;
    # this point with $x bound to 10
    yield $x+1;
    return 5;
    ... # this is never reached, I think we all agree
}
