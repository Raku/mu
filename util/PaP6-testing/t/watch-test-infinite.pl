use strict;
use Time::HiRes qw ( sleep );

my $no_rec = $ARGV[0];
#print "--- $no_rec ---\n";

my $to = int rand(5)+2;
print "to: $to (PID: $$)\n";
foreach (1..$to) {
    print "$_ ";
    print "pid: $$ " unless $no_rec;
    print ( "." x 70 );
    print "\n";
    sleep( int(rand(100)+100)/1000 );
    unless ( $no_rec ) {
        my $finite = int( (rand 1) + 0.8 );
        print "creating new child, finite=$finite\n\n";
        system( "perl \"$0\" $finite" );
        print "returned to PID: $$\n";
    }
}

if ( $no_rec ) {
    print "PID: $$ finished\n\n";
    exit 0;
}

print "PID: $$ infinite loop has started\n";
for (1..100) {
    print ( "infinite loop " x 10 );
    print "\n";
}
print "\n";

while (1) { sleep 60; };