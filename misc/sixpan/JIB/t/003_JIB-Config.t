use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class = 'JIB::Config';

use_ok( $Class );

### create an object 
my $Obj;
{   $Obj = $Class->new;
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               $Class );
    
    my @can = sort $Obj->ls_accessors;
    for my $method ( @can ) {
        my $rv = $Obj->$method;
        ok( $rv,                "   '$method' returns value '$rv'" );
    }        
    
    is( $Obj, $Class->new,      "Same object returned on 2nd call" );
}    

    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
