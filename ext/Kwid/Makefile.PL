use strict;
use Config;
use inc::Module::Install;

my $kwid = "kwid$Config{_exe}";

name            ('Kwid');
version         ('0.0');
abstract        ('Kwid');
author          ('Brian Ingerson <ingy@cpan.org>');
license         ('perl');
install_script  ($kwid);
makemaker_args  (
    test => { TESTS => join ' ', (glob("t/*.t"), glob("t/*/*.t")) },
    MAN1PODS => {},
);

# find subdirs in src
my @srcdirs = ("src", grep(-d, glob "src/*"));

clean_files();

my @srcfiles = map glob("$_/*.hs"), @srcdirs;
postamble(<< ".");

$kwid :: @srcfiles
	ghc --make -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing -o $kwid src/Main.hs -isrc

.

WriteAll( sign => 1 );
