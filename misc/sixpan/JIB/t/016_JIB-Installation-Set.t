use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };
BEGIN { require 'conf.pl' }

use JIB::Config;
my $Conf    = JIB::Config->new;
my $Class   = 'JIB::Installation::Set';
my @Acc     = qw[installations];


use_ok( $Class );


### create an object 
my $Obj;
{   $Obj = $Class->new( dirs => [$INSTALLATION_DIR] );
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               $Class );
    can_ok( $Obj,               @Acc );

    for my $inst ( @{ $Obj->installations } ) {
        isa_ok( $inst,          "JIB::Installation" );
    }
}




# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
