use strict;

### fixes the config for us
BEGIN { require 'conf.pl' }
BEGIN { require 'pkg.pl' }

use JIB::Config;
use JIB::Repository;

use Path::Class ();
use Cwd;
use vars qw[$REPO];

$REPO = JIB::Repository->new( root => $REPOSITORY_ROOT )
                or die "No repository created";

$REPO->create   or die "Creation failed";

for my $jib ( values %PKGS ) {
    $REPO->add_file( file => $jib->file ) or die "Could not add $jib";
}    

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
