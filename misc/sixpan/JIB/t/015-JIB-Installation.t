use Test::More 'no_plan';
use strict;
use File::Spec;

BEGIN { chdir 't' if -d 't' };
BEGIN { use lib qw[../lib inc] };
BEGIN { require 'conf.pl' }

my $Class = 'JIB::Installation';

use_ok( $Class );

### XXX test accessors

### create an object 
my $Obj;
{   $Obj = $Class->new;
    ok( $Obj,                   "Object created" );
    isa_ok( $Obj,               $Class );
}    
    
    
    
# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
