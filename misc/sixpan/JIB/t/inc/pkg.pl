use strict;

### fixes the config for us
BEGIN { require 'conf.pl' }

use JIB::Config;
use JIB::Source;

use Path::Class ();
use Cwd;
use vars qw[@PKGS %PKGS];

for ( qx[ls -1 src] ) {
    chomp;
    my $dir = Path::Class::dir( 'src', $_ );
    
    my $src = JIB::Source->new( dir => $dir ) 
        or die "Could not create JIB::Source object from '$dir'";
        
    my $pkg = $src->build 
        or die "Could not build source package from '$dir'";
        
    push @PKGS, $pkg;
}    

%PKGS = map { $_->package => $_ } @PKGS;

1;


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
