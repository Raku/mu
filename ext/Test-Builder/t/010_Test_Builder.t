#!/usr/bin/pugs

use v6;
use Test;

plan 5;

use Test::Builder;

my $Test = Test::Builder.new();
is( $Test.ref, 'Test::Builder', '... we got a Test::Builder instance' );

{
    my $Test2 = Test::Builder.new();
    ok( $Test =:= $Test2, '... Test::Builder is a singleton', :todo<feature> );
}

	class Test::Builder::CustomPlan is Test::Builder::TestPlan
	{
		method footer returns Str ( Int $run )
		{
			return 'ok 5 - DESTROY should write plan footer, if it exists';
		}
	}

{
	my $Test3;
	lives_ok
	{
		my $custom_plan = Test::Builder::CustomPlan.new();
		$Test3          = Test::Builder.create( plan => $custom_plan );
		is( ! $Test3 =:= $Test,
		      'create() should return non-singleton instance'
		);
	}, 'create() should be accessible with TestPlan subclass',
	:todo<feature>;

	# hopefully launch DESTROY()

	for 1 .. 100
	{
		$Test3 = Test::Builder.create();
	}
}
