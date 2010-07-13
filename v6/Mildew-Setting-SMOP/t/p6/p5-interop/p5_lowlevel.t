say "1..3";

EXTERNAL::eval_perl5('$| = 1');
$OUT.unbuffered;

my $foo = EXTERNAL::eval_perl5('
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
sub test3_str {
    my $str = "$_[1]";
    if ($str eq "abcdefgh?!") {
        print "ok 3\n";
    } else {
        print "not ok 3\n";
    }
}
bless {},"Foo";
');
$foo.test1_bool(139);
$foo.test2_bool(0);
$foo.test3_str("abcdefgh?!");
