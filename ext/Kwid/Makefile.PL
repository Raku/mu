use strict;
use Config;
use inc::Module::Install;
use FindBin;

my $kwid = "kwid$Config{_exe}";

name            ('Kwid');
version         ('0.0');
abstract        ('Kwid');
author          ('Brian Ingerson <ingy@cpan.org>');
license         ('perl');
install_script  ("$FindBin::Bin/$kwid");
blib_depth      (2);

postamble(<< ".");

$kwid :: src/Kwid/AST.hs
	ghc --make -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing -o $FindBin::Bin/$kwid $FindBin::Bin/src/Main.hs -i"$FindBin::Bin/src"

.

WriteAll( sign => 1 );
pugs_fix_makefile();
