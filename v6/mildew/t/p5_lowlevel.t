$OUT.print("1..1\n");
my $p5 = ::P5Interpreter.new();

$p5.eval('$| = 1');
$OUT.unbuffered;

my $foo = $p5.eval('
package Foo;
use strict;
sub pass_through {
    my ($self,$arg) = @_;
    return $arg;
}
sub test1_bool {
    if ($_[1]) {
        print "ok 1\n";
    } else {
        print "not ok 1\n";
    }
}
sub test2_bool {
    if ($_[1]) {
        print "not ok 2\n";
    } else {
        print "ok 2\n";
    }
}
bless {},"Foo";
');
$foo.test1_bool(139);
$foo.test2_bool(0);
