use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };

my $Class   = 'JIB::Package';
my @Acc     = qw[package];
my $Pkg     = 'p5-foo-1-cpan+KANE';
my @Parse   = qw[prefix name version authority];

use_ok( $Class );

### create an object 
my $Obj;
{   $Obj = $Class->new( package => $Pkg );
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
 
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
