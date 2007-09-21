
use KindaPerl6::Runtime::Perl6::Scope;

say '1..1';

my $s = Scope.new( vars => {} );

say 'ok 1 # load Scope.pm';

$s.create( '$abc' );
$s.create( '$def' );

$s.LOOKUP( '$abc' );
