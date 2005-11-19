#!/usr/bin/pugs
use v6;
use Test;


# These tests are not specified by p6l, But these should be right...

plan 8;

diag('Test for class attribute initializement');

{
	eval_ok q|class T1 { has $.t = 1 }; 1|,
		"Try to initialize public attribute",
		:todo<unspecced>;

	eval_ok q|
		class T2{
		has $!t = 2;
		method get { $!t };
			}; 1 |,
		"Try to initialize private attribute",
		:todo<unspecced>;


skip_rest "Not implemented...";
	my T1 $o1;
	my T2 $o2;

	$o1 = T1.new();
	$o2 = T2.new();
	is $o1.t, 1,
		"Testing value for initialized public attribute.";
	dies_ok { $o2.t },
		"Try to access the initialized private attribute.";
	is $o2.get,2,
		"Testing value for initialized private attribue.";

	$o1 = T1.new( t => 3 );
	$o2 = T2.new( t => 4 );
	is $o1.t, 3,
		"Testing value for attributes which is initialized by constructor.";
	dies_ok { $o2.t },
		"Try to access the private attribute which is initialized by constructor.";
	is $o2.get,4,
		"Testing value for private attribue which is initialized by constructor.";
}
