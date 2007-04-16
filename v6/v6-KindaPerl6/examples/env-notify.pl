# create variables in a pad; monitor the pad for changes
# dump the changes only

use KindaPerl6::Perl5::Pad;
use KindaPerl6::Perl5::Type;

package main;
use Data::Dump::Streamer;

use strict;

package Decl;
sub var { $_[0]->{var} }
sub emit_perl5 { q/ 
    my $x = bless [ \( do{ my $v } ), \%_MODIFIED, '$x' ], "Type_Scalar" 
/ }
#sub emit_perl5 { q/ my $x = bless \( do{ my $v } ), "Type_Scalar" / }
package Var;
sub emit_perl5 { '$x' }

package main;

my $node = bless { decl => 'my', type => '', var => 
    bless { sigil => '$', twigil => '', name => 'x' }, 'Var' }, 'Decl';

my $env1 = Pad->new( outer => undef, lexicals => [ $node ] );

$env1->eval( q/ $x->STORE( bless \( do{ my $v = 123 } ), 'Type_Constant_Int' ) / );
$env1->eval( ' print "x= ",$x->FETCH, "\n" ' );   
#$env1->eval( q/ $x->STORE( $x->FETCH + 1 ) / );
print "Var = ", $env1->eval( ' $x->FETCH ' ), "\n";   

print $env1->eval( q/ use Data::Dumper; Dumper( \%_MODIFIED ) / ), "\n";
print $env1->eval( q/ use Data::Dumper; Dumper( \$x ) / ), "\n";

__END__

my $env2 = Pad->new( $env1, ['$y'] );

$env2->eval( '$y = 42' );
$env2->eval( ' print "y=$y\n" ' );   

my $env3 = Pad->new( $env2, ['$z'] );

__END__

{
    my ($x,$y);
    
    $x = bless \( do{ my $v } ), 'TypeInt';
    
    $y = $x; 
    $x->STORE( 3 ); 
    #print "x is a ",ref($x),"\n", Dump($x);
    #print "y is a ",ref($y),"\n", Dump($y);
    
    print $$y, " typed y (1)\n"; 
    $y->STORE( 4 ); 
    print $$x, "\n";
    
    $y->STORE( 42 );
    print $$y, " typed y (2)\n"; 
    print $$x, "\n";
    $y->STORE( 'a' );
    
    $x->STORE( 99 );
    print $$y, " typed x (3)\n"; 
    print $$x, "\n";
    $x->STORE( 'a' );
    
}

{
    my ($x,$y);

    $x = bless [ ], 'TypeIntArray';

    $y = $x; 
    $x->INDEX( 0 )->STORE( 3 ); 
    #print "x is a ",ref($x),"\n", Dump($x);
    #print "y is a ",ref($y),"\n", Dump($y);
    
    print ${$x->INDEX( 0 )}, " typed y (1)\n"; 
    $y->INDEX( 0 )->STORE( 4 ); 
    print ${$x->INDEX( 0 )}, "\n";
    
    $y->INDEX( 0 )->STORE( 42 );
    print ${$y->INDEX( 0 )}, " typed y (2)\n"; 
    print ${$x->INDEX( 0 )}, "\n";
    $y->INDEX( 0 )->STORE( 'a' );
    
    $x->INDEX( 0 )->STORE( 99 );
    print ${$y->INDEX( 0 )}, " typed x (3)\n"; 
    print ${$x->INDEX( 0 )}, "\n";
    $x->INDEX( 0 )->STORE( 'a' );

}
