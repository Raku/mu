#!/usr/bin/perl

use strict;
use inc::Module::Install;

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ('pugs');
build_requires  ('Test::More');

can_run('ghc') or die << '.';
*** Cannot find a runnable 'ghc' from path.
*** Please install GHC from http://haskell.org/ghc/.
.

clean_files(glob("src/*.hi"), glob("src/*.o*"), glob("pugs*"), "config.h");

my $has_readline = 1;
$has_readline = 0 if $^O eq 'MSWin32';

if ($^O =~ /darwin/i and not -e "/usr/local/readline/readline.h") {
    die << '.';
*** /usr/local/readline/readline.h not found.
*** Readline support disabled.  If you want readling support,
    please install Readline from the URL below and try again:
    http://p4.elixus.org/snap/pugs/macosx-readline-5.0.005.pkg.tar.gz
.
}


open FH, '> config.h' or die $!;
if ($has_readline) {
    print FH "#define READLINE 1\n";
}
else {
    print FH "#undef READLINE\n";
}
close FH;

postamble(<< ".");
pugs: @{[glob("src/*.hs")]}
	ghc --make -o pugs -O src/Main.hs -isrc

tags ::
	hasktags -c src
.

WriteAll( sign => 1 );
