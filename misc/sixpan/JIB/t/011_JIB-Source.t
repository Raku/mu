use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };
BEGIN { require 'conf.pl' }

my $Class = 'JIB::Source';
### XXX generate from somewhere
my %Map   = (
    dir     => sub { -d shift() },
    meta    => sub { UNIVERSAL::isa( shift(), 'JIB::Meta' ) },
    config  => sub { UNIVERSAL::isa( shift(), 'JIB::Config' ) },
);
my @Acc   = sort keys %Map;

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

    while( my($meth, $check) = each %Map ) {
        ok( $check->( $Obj->$meth ), 
                                "       '$meth' of proper value" );
    }
}    
    
### build a package
{   my $archive = $Obj->build;
    ok( $archive,               "Archive '$archive' created" );
    ok( -e $archive,            "   File exists" );

}
    
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
