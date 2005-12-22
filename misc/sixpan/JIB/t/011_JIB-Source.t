use Test::More 'no_plan';
use strict;
use Path::Class;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };
BEGIN { require 'conf.pl' }

use JIB::Constants;

my $Class = 'JIB::Source';
### XXX generate from somewhere
my %Map   = (
    dir     => DIR_EXISTS,
    meta    => ISA_JIB_META,
    config  => ISA_JIB_CONFIG,
);
my @Acc   = sort keys %Map;

### XXX config
my $Dir   = dir( qw[src p5-b] );

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

    while( my($meth, $check) = each %Map ) {
        ok( $check->( $Obj->$meth ), 
                                "       '$meth' of proper value" );
    }
}    
    
### build a package
{   my $pkg = $Obj->build;
    ok( $pkg,                   "Archive created" );
    isa_ok( $pkg,               'JIB::Package' );
    ok( -e $pkg->file,          "   File exists" );

}
    
    
    
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
