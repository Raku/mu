#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

Test handling of C<cwd>.

=cut

use File::Spec; pass "(dummy instead of broken use_ok)";

# XXX: this function does not work on Win32 either.
sub manual_cwd () {
  # This HACK is worse than
  # the File::Spec platform hack
  if ($?OS eq 'MSWin32') {
    my @retval = system("cd");
    my $cwd = @retval[0];
    $cwd .= chomp;
    return $cwd;
  }
# This doesn't work - matter of fact it never did :)
#   else {
#     system("pwd")
#   }
}

if ($?OS eq 'MSWin32') {
    like(cwd(), rx:perl5{\\}, "cwd() returns a file like value in Win32");
}
else {
    like(cwd(), rx:perl5{^\/}, "cwd() returns a file like value in Unix");
}
