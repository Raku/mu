#!/usr/bin/pugs
# This is a *evil* example of random operator overloading.

use v6;

BEGIN {
  # At compilation time, we randomly pick a op to be overloaded.
  my $op = pick <+ - * />; #/#--vim

  # Now, we substitute this op with the constant value 42.
  eval "sub infix:<$op> \{ 42 \}";
  # "Why hadn't you used my &::("infix:<$op>") here?"
  # -- Several reasons:
  #    - Internally, an operator's name is currently stored as (say) "infix:+"
  #      (note the missing brackets).
  #    - my() expects a ruleVarName (ruleVarName :: RuleParser String), but the
  #      rule which is reponsible for the symbolic dereferentiation is
  #      incompatible to that type (ruleSymbolicDeref :: RuleParser Exp).
}

# Now one of +-*/ will return 42, the other ops will continue to work as
# normal.
say "1 + 1 = {1 + 1}";
say "1 * 1 = {1 * 1}";
say "1 / 1 = {1 / 1}";
say "1 - 1 = {1 - 1}";
