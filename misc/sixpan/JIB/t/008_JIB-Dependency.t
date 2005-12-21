use Test::More 'no_plan';
use strict;
use Path::Class ();

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class   = 'JIB::Dependency';
my $Meta    = 'JIB::Meta';
my @Acc     = qw[struct];
my $Expect  = 'p5-a AND (p5-b >= 3 OR p5-b < 2) AND p5-c AND (p5-e OR p5-d)';
### XXX Config
my $File    = Path::Class::file( qw[src p5-complicated-deps _jib META.info] );

use_ok( $Class );
use_ok( $Meta );

### create an object 
my $Obj;
{   my $meta_obj = $Meta->new( file => $File );
    ok( $meta_obj,              "Object created" );
    isa_ok( $meta_obj,          $Meta );

    {   my $acc = 'depends';
        can_ok( $meta_obj,      $acc );

        $Obj = $meta_obj->$acc;
        isa_ok( $Obj,           $Class );
    }
    
    my @can = sort $Obj->ls_accessors;
    ok( scalar(@can),           "   Object has accessors" );
    is_deeply( \@can, \@Acc,    "   Object can do what it should" );

    for my $method ( @can ) {
        ok( $Obj->$method,      "       '$method' returns value" );
    }        
}    


### pretty_print tests
{   my $meth = 'pretty_print';
    can_ok( $Obj,               $meth );
    
    my $str = $Obj->$meth;
    ok( $str,                   "   '$meth' returns string" );
    is( $str, $Expect,          "   Got '$Expect'" );

}

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
