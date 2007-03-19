
# file Pugs::Runtime::Perl6AST
use v6-alpha;

module v6::AST;

use v5;
$INC{"v6/AST.pm"}=1;  # mark v6::AST as "used"
use v6;

sub node( $match, $type ) {
    #say "new node $match $type\n";
    use v5; 
    $type->create( $match )
        if ref $match;   # ???
    use v6;
}

module v6::AST::Base;

use v5; 
$INC{"v6/AST/Base.pm"}=1;  # mark v6::AST::Base as "used"
use Data::Dumper;
sub create {
    #print "create new node $_[0], ", Dumper( $_[1]->data );
    #print " str ", $_[1]->flat, "\n";
    bless \( $_[1]->flat ), $_[0];
}
sub str {
    #print Dumper( ${$_[0]} );
    my $s = ${$_[0]};  
    return $s;
}
use v6; 

