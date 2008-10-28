
use Test::More tests => 8;
BEGIN { use_ok('SMOP') };

my $r;
eval '$r = SMOP::NATIVE::bool->true';
ok(!$@,'Can call SMOP::NATIVE::bool->true');
is(ref($r),'SMOP::Object','returns a SMOP::Object');

eval '$r = SMOP::NATIVE::bool->false';
ok(!$@,'Can call SMOP::NATIVE::bool->false');
is(ref($r),'SMOP::Object','returns a SMOP::Object');

eval '$r = SMOP::NATIVE::int->create(4)';
ok(!$@,'Can call SMOP::NATIVE::int->create');
is(ref($r),'SMOP::Object','returns a SMOP::Object');

is($r->SMOP::NATIVE::int::fetch, 4, 'fetches the int');