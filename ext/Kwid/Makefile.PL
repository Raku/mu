use strict;
use File::Basename;
my $Bin; 
BEGIN { $Bin = dirname(__FILE__) };
use lib "$Bin/../..", "$Bin/../../inc";
use inc::Module::Install;
use Config;

my $kwid = "kwid$Config{_exe}";

name            ('Kwid');
version         ('0.01');
abstract        ('Kwid');
author          ('Brian Ingerson <ingy@cpan.org>');
license         ('perl');
install_script  ('kwid');

postamble(nativize(<< "."));

$Bin/kwid :: src/Kwid/AST.hs
	ghc --make -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing -o $Bin/$kwid $Bin/src/Main.hs -i"$Bin/src"

.

set_blib(6);
WritePugs();
