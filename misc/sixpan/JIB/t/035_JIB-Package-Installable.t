use Test::More 'no_plan';
use Test::NoWarnings;
use strict;
use Path::Class;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };
BEGIN { require 'conf.pl' }
BEGIN { require 'pkg.pl' }
BEGIN { require 'repo.pl' }

use JIB::Config;

my $Conf    = JIB::Config->new;
my $Class   = 'JIB::Package';
my @Acc     = sort qw[package file config meta repository];

### XXX config
my $Pkg     = $PKGS{'p5-Foo-Bar-1.2-cpan+KANE'};
my $File    = $Pkg->file;

use_ok( $Class );

### create an object 
my $Obj;
{   ### XXX ugly amount of args
    $Obj = $Class->new(file => $File, meta => $Pkg->meta, repository => $REPO);
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               'JIB::Package::Installable' );
    
    my @can = sort $Obj->ls_accessors;
    ok( scalar(@can),           "   Object has accessors" );
    is_deeply( \@can, \@Acc,    "   Object can do what it should" );
    
    for my $method ( @can ) {
        ok( $Obj->$method,      "   '$method' returns value" );
    }        
}    

### sanity checks
{   is($Obj->repository, $REPO, "Repository linked from object" );
    
}    
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
