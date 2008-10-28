
use Test::More tests => 3;
BEGIN { use_ok('SMOP') };


eval 'my $r = SMOP::NATIVE::bool->true';
ok(!$@,'Can call SMOP::NATIVE::bool->true');

is(ref(SMOP::NATIVE::bool->true),'SMOP::Object','returns a SMOP::Object');
