
use KindaPerl6::Runtime::Perl6::Scope;

say '1..4';

my $s = Scope.new( vars => {} );

say 'ok 1 - load Scope.pm';

$s.create( '$abc' );
$s.create( '$def' );

$s.LOOKUP( '$abc' ) = 123;

if $s.LOOKUP( '$abc' ) == 123 {
    say 'ok 2 - lookup';
}
else {
    say 'not ok 2';
};

my $s2 = $s.inner;

if $s2.LOOKUP( '$abc' ) == 123 {
    say 'ok 3 - inner';
}
else {
    say 'not ok 3';
};

if $s2{'$abc'} == 123 {
    say 'ok 4 - Scope behaves like Hash';
}
else {
    say 'not ok 4';
};

