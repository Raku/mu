
use Test::More tests => 13;
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

eval '$r = SMOP::NATIVE::idconst->create("Hello World!")';
ok(!$@,'Can call SMOP::NATIVE::idconst->create');
is(ref($r),'SMOP::Object','returns a SMOP::Object');
is($r->SMOP::NATIVE::idconst::fetch, 'Hello World!', 'fetches the idconst');

eval '$r = SMOP::S1P->Scalar()';
ok(!$@,'Can call SMOP::S1P->Scalar');
is(ref($r),'SMOP::Object','returns a SMOP::Object');
