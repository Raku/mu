use strict;
use Config;
use File::Basename;
my $Bin; BEGIN { $Bin = dirname(__FILE__) };
use lib "$Bin/../..", "$Bin/../../inc";
use inc::Module::Install;

my $kwid = "kwid$Config{_exe}";

name            ('Kwid');
version         ('0.0');
abstract        ('Kwid');
author          ('Brian Ingerson <ingy@cpan.org>');
license         ('perl');
install_script  ('kwid');
blib_depth      (2);

postamble(<< ".");

$Bin/kwid :: src/Kwid/AST.hs
	ghc --make -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing -o $Bin/$kwid $Bin/src/Main.hs -i"$Bin/src"

.

WriteAll( sign => 1 );
pugs_fix_makefile();
