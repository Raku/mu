#!/usr/bin/pugs

use v6;
require Test;

plan 2;

=pod

Test handling of C<cwd>.

=cut

use_ok('File::Spec');

sub manual_cwd () {
  # This HACK is worse than
  # the File::Spec platform hack
  if ($?OS eq 'MSWin32') {
    my @retval = system("cd");
    my $cwd = @retval[0];
    chomp($cwd);
    return $cwd;
  }
# This doesn't work - matter of fact it never did :)
#   else {
#     system("pwd")
#   }
}

if ($?OS eq 'MSWin32') {
    is(cwd(), manual_cwd(), "cwd() returns the same as the manual implementation in Win32");
}
else {
    like(cwd(), rx:perl5{^\/}, "cwd() returns a file like value in Unix");
}