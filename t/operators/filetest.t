#!/usr/bin/pugs

use v6;
require Test;

=head1 DESCRIPTION

This test tests the various filetest operators.

=cut

plan 24;

# Basic tests
ok            -d 't/',    "-d returns true on directories";
ok            -f "pugs",  "-f returns true on files";
todo_eval_ok "-e 'pugs'", "-e returns true on files";
todo_eval_ok "-e 't/'",   "-e returns true on directories";
todo_eval_ok "-r 'pugs'", "-r returns true on readable files";
todo_eval_ok "-w 'pugs'", "-w returns true on writable files";

if($?OS eq any<MSWin32 mingw cygwin>) {
  skip 2, "win32 doesn't have -x";
} else {
  todo_eval_ok "-x 'pugs'", "-x returns true on executable files";
  todo_eval_ok "-x 't/'",   "-x returns true on cwd()able directories";
}

ok           !-f "t/",  "-f returns false on directories";
todo_eval_ok '-r "t/"', "-r returns true on a readable directory";

if($?OS eq any<MSWin32 mingw cygwin>) {
  skip 2, "win32 doesn't have /etc/shadow";
} else {
  todo_eval_ok '!-r "/etc/shadow"', "-r returns false on unreadable files";
  todo_eval_ok '!-w "/etc/shadow"', "-w returns false on unwritable files";
}

ok            !-d 'doesnotexist/',  "-d returns false on non existant directories";
todo_eval_ok "!-r 'doesnotexist/'", "-r returns false on non existant directories";
todo_eval_ok "!-w 'doesnotexist/'", "-w returns false on non existant directories";
todo_eval_ok "!-x 'doesnotexist/'", "-x returns false on non existant directories";
ok            !-f 'doesnotexist/',  "-f returns false on non existant directories";

ok            !-f 'doesnotexist.t',  "-f returns false on non existant files";
todo_eval_ok "!-r 'doesnotexist.t'", "-r returns false on non existant files";
todo_eval_ok "!-w 'doesnotexist.t'", "-w returns false on non existant files";
todo_eval_ok "!-x 'doesnotexist.t'", "-x returns false on non existant files";
ok            !-f 'doesnotexist.t',  "-f returns false on non existant files";

todo_eval_ok '-s "pugs" > 42',       "-s returns size on existant files";
todo_eval_ok '!-s "doesnotexist.t"', "-s returns undef on non existant files";

# Yet missing: stacked filetests
