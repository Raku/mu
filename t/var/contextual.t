use v6-alpha;

use Test;

plan 4;

%*ENV<THIS_NEVER_EXISTS> = 123;

{
	is $+THIS_NEVER_EXISTS, 123, "Testing contextual variable which changed within %*ENV";
}

{
	delete %*ENV<THIS_NEVER_EXISTS>;
	eval_ok '$+THIS_NEVER_EXISTS', "Testing for accessing contextual which is deleted.";
	is $+THIS_NEVER_EXISTS, undef, "Testing for value of contextual varaibale.";
}

{
	eval_ok '$+ABCDEFG', "Test for contextual which doesn't exists.", :todo<bug>;
#	is $+ABCDEFG, undef, "Test for contextual which doesn't exists.";
}

