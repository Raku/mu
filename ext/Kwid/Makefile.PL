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
blib_depth      (2);

postamble(<< ".");

$kwid :: src/Kwid/AST.hs
	ghc --make -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing -o $kwid src/Main.hs -isrc

.

WriteAll( sign => 1 );
pugs_fix_makefile();
