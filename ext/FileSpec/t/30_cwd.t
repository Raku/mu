#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<cwd>.

=cut

require File::Spec;

sub manual_cwd () {

  # This HACK is worse than
  # the File::Spec platform hack
  if ($?OS eq 'MSWin32') {
    my @retval = system("cd");
    my $cwd = @retval[0];
    chomp($cwd);
    return $cwd;
  } else {
    system("pwd")
  }
};

plan(1);

is(cwd, manual_cwd, "cwd() returns the same as the manual implementation")