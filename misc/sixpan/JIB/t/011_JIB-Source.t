use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class = 'JIB::Source';
my @Acc   = sort qw[dir meta config];

### XXX config
my $Dir   = File::Spec->catdir( qw[src p5-b] );

use_ok( $Class );

### create an object 
my $Obj;
{   $Obj = $Class->new( dir => $Dir );
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               $Class );
    
    my @can = sort $Obj->ls_accessors;
    ok( scalar(@can),           "   Object has accessors" );
    is_deeply( \@can, \@Acc,    "   Object can do what it should" );

    for my $method ( @can ) {
        ok( $Obj->$method,      "       '$method' returns value" );
    }        
}    
    
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
