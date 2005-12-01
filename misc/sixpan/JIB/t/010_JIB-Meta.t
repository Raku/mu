use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class = 'JIB::Meta';

### XXX Config
my $File  = File::Spec->catfile( qw[src p5-b _jib META.info] );

### XXX config
my @Acc   = sort qw[author description package name version authority provides];

use_ok( $Class );

### create an object 
my $Obj;
{   $Obj = $Class->new( file => $File );
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
