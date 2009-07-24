use Test;
is 5,eval("5");
my $foo = 7;
is 7,eval('$foo');
done_testing;
