#!/usr/bin/pugs

use v6;
use Test;

plan 4;

use Test::Builder;
use Test::Builder::TestPlan;

my $Test = Test::Builder.new();
is( $Test.ref, 'Test::Builder', 'new() should return a Test::Builder object' );

{
    my $Test2 = Test::Builder.new();
    ok( $Test =:= $Test2, '... Test::Builder is a singleton' );
}

class Test::Builder::CustomPlan is Test::Builder::NullPlan
{
	method footer returns Str ( Int $run )
	{
		return 'ok 4 - DESTROY should write plan footer, if it exists';
	}
}

{
	my $custom_plan = Test::Builder::CustomPlan.new();
	my $Test3       = Test::Builder.create( plan => $custom_plan );
	isnt( $Test3.id, $Test.id, 'create() should return non-singleton object' );

	# hopefully launch DESTROY()
	for 1 .. 100
	{
		$Test3 = Test::Builder.create();
	}
}
