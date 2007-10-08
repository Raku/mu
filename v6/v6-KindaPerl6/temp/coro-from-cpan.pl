use strict;
use Coro;

my %GATHER;  # holds the inside-out-ish lazy list instances

sub take {
  #print " current $Coro::current \n";
  push @{ $GATHER{ $Coro::current } }, $_[0];
}

sub push_one { 
  take( int(rand(100)) );
  cede;
}

sub gather_taker {
 my $gather_finished; 
 my $gather_coro = Coro::async {
   sub {
   for (0..10) {
     take( int(rand(100)) );
     cede;
     push_one;
   };
   }->();
   delete $GATHER{ $Coro::current };   # cleanup the pointer to the lazy buffer
   $gather_finished = 1;
   return;
 };
 #print "Coro: $gather_coro \n";
 $GATHER{ $gather_coro } = [];
 return ( $GATHER{ $gather_coro }, \$gather_finished );
}
 
my ( $gather, $gather_finished ) = gather_taker();
my ( $gather2, $gather_finished2 ) = gather_taker();
while (! $$gather_finished ) {
   print "1 $gather [ @$gather ]\n";
   cede;
   print "2 $gather2 [ @$gather2 ]\n";
   cede;
}

