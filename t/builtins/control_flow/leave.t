#!/usr/bin/pugs

use v6;
use Test;

# Basic &leave tests
# L<S06/"The leave function">

plan 5;

flunk "leave() not implemented in Pugs's interpreter core yet", :todo<feature>;
skip_rest; exit;

{
  my $bare = { leave 42; 23 };

  is $bare(), 42, "basic leave() works";
}

{
  my $sub = sub () {
    my $bare = { leave 42; 23 };

    my $ret = $bare();
    return 1000 + $ret;
  };

  is $sub(), 1042, "leave() works and leaves only the innermost block";
}

{
  my $sub = sub () {
    leave &?SUB, 42;
    return 23;
  };

  is $sub(), 42, "leave() works with &?SUB as parameter";
}

{
  my $outer = sub () {
    my $inner = sub () {
      my $most_inner = sub () {
        leave $outer, 42;
        return 23;
      };

      $most_inner();
      return 22;
    };

    $inner();
    return 21;
  }

  is $outer(), 42, "nested leave() works with a subref as parameter";
}

{
  my $sub = sub () {
    my $bare = sub () {
      leave Block, 42;
      return 23;
    };

    my $ret = $bare();
    return 1000 + $ret;
  };

  is $sub(), 1042, "leave() works with a Class (Block) as parameter";
}

{
  my $sub = sub () {
    my $bare = sub () {
      leave Sub, 42;
      return 23;
    };

    my $ret = $bare();
    return 1000 + $ret;
  };

  is $sub(), 42, "leave() works with a Class (Sub) as parameter";
}
