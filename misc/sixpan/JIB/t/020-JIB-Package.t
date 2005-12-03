use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class   = 'JIB::Package';
my @Acc     = sort qw[package file config];

### XXX config
my $Pkg     = 'p5-b-1-cpan+KANE';
my $File    = File::Spec->catfile( 'src', $Pkg.'.jib' );
my @Parse   = qw[prefix name version authority];

use_ok( $Class );

### XXX package->new now takes a jib file as argument
### need to fix tests for that

### create an object 
my $Obj;
{   $Obj = $Class->new( file => $File );
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               $Class );
    
    my @can = sort $Obj->ls_accessors;
    ok( scalar(@can),           "   Object has accessors" );
    is_deeply( \@can, \@Acc,    "   Object can do what it should" );
    
    for my $method ( @can ) {
        ok( $Obj->$method,      "   '$method' returns value" );
    }        
}    

### test bogus assign to package
{   local $SIG{__WARN__} = sub {};
    ok( !$Obj->package( [] ),   "Bogus 'package' value not allowed" );
}

### test regexes
{   ### build a method => value map
    my $i;
    my %map = map { $Parse[$i++] => $_ } split /-/, $Pkg;
 
    for my $method ( keys %map ) {
        can_ok( $Obj,           $method );
        
        my $rv = $Obj->$method;
        ok( $rv,                "   Returns '$rv'" );
        is( $rv, $map{$method}, "       As expected" );
    }
}
 
### installl the package
{   my $rv = $Obj->install;
    ok( $rv,                    "Package installed" );
    
    ### XXX add file tests
}    

 
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
