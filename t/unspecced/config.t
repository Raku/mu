#!/usr/bin/pugs

use v6;
require Test;

=pod

Basic tests of C<< %*CONFIG >>, the equivalent to
C<Config.pm>. Most of this is not yet even decided on,
so all of this test can become obsolete on Larrys whim C<:)>

=cut

plan 2;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");
if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
};

todo_eval_ok( 'defined %?CONFIG', '%?CONFIG is defined' );
todo_eval_ok( '%?CONFIG.keys() > 0', '%?CONFIG contains keys and values' );
# todo_eval_ok( 'defined %?CONFIG{path_sep}', '%?CONFIG{path_sep} is defined' );
