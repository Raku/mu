say "1..3";

EXTERNAL::eval_perl5('$| = 1');
$OUT.unbuffered;

my $foo = EXTERNAL::eval_perl5('
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
my $var2 = EXTERNAL::eval_perl5('"ok 2\n"');
my $var3;
$foo.test_fetch($var1);
$foo.test_fetch($var2);
$foo.test_store($var3);
$foo.test_fetch($var3);
