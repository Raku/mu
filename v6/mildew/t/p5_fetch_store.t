$OUT.print("1..1\n");
my $p5 = ::P5Interpreter.new();

$p5.eval('$| = 1');
$OUT.unbuffered;

my $foo = $p5.eval('
package Foo;
use strict;
sub test_fetch {
    my $fetched = $_[1];
    print $fetched;
    return;
}
sub test_store {
    $_[1] = "ok 3\n";
    8;
}
bless {},"Foo";
');
my $var1 = "ok 1\n";
my $var2 = $p5.eval('"ok 2\n"');
my $var3;
$foo.test_fetch($var1);
$foo.test_fetch($var2);
$foo.test_store($var3);
$foo.test_fetch($var3);
