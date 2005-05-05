#!/usr/bin/pugs

use v6;
use Test;

plan 7;

# L<S04/"The Relationship of Blocks and Declarations" /There is a new state declarator that introduces/>

# state() inside coderefs
{
  my $gen = {
    # Note: The following line is only executed once, because it's equivalent
    # to
    #   state $svar will first{ 42 };
    # See L<S04/"Closure traits" /emantics to any initializer, so this also works/>
    state $svar = 42;
    my $ret = { $svar++ };
  };

  my $a = $gen(); # $svar == 42
  $a(); $a();     # $svar == 44
  my $b = $gen(); # $svar == 44

  is $b(), 44, "state() works inside coderefs";
}

# state() inside subs
{
  sub gen {
    state $svar = 42;
    my $ret = { $svar++ };
  };

  my $a = gen(); # $svar == 42
  $a(); $a();    # $svar == 44
  my $b = gen(); # $svar == 44

  is $b(), 44, "state() works inside subs";
}

# state() inside for-loops
{
  for [1,2,3] -> $val {
    state $svar = 42;
    $svar++;

    # Only check on last run
    if($val == 3) {
      is $svar, 45, "state() works inside for-loops";
    }
  }
}

# state will first{...}
{
  my $gen = eval '{
    state $svar will first { 42 };
    my $ret = { $svar++ };
  }';

  my $a = gen(); # $svar == 42
  $a(); $a();    # $svar == 44
  my $b = gen(); # $svar == 44

  is $b(), 44, 'state will first{...} works';
}

# Return of a reference to a state() var
{
  my $gen = {
    state $svar = 42;
    \$svar;
  };

  my $svar_ref = $gen();
  $svar_ref++; $svar_ref++;

  my $svar_ref = $gen();
  is $svar_ref, 44, "reference to a state() var";
}

# Anonymous state vars
# http://groups.google.de/group/perl.perl6.language/msg/07aefb88f5fc8429
{
  my $gen = { try { \state } };

  my $svar_ref = $gen();    # $svar == 0
  $svar_ref++; $svar_ref++; # $svar == 2

  my $svar_ref = $gen();    # $svar == 2
  is $svar_ref, 2, "reference to a state() var";
}

# http://www.nntp.perl.org/group/perl.perl6.language/20888
# ("Re: Declaration and definition of state() vars" from Larry)
{
  my $gen = {
    (state $svar) = 42;
    my $ret = { $svar++ };
  };

  my $a = gen();    # $svar == 42
  $a(); $a();       # $svar == 44
  my $b = gen();    # $svar == 42
  is $b(), 42, "state() and parens";
                    # $svar == 43
}
