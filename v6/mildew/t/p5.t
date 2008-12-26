$OUT.print("1..7\n");
my $p5interpreter = ::P5Interpreter.new();

$p5interpreter.eval('$| = 1');
$OUT.unbuffered;

$p5interpreter.eval('print "ok 1\n"');
my $foo = $p5interpreter.eval('"ok 2\n";');
$OUT.print($foo.Str);
my $closure = $p5interpreter.eval('print "ok 3\n";sub {print "ok 5 # from p5 sub\n"}');
$OUT.print("ok 4 # smop lives after p5 sub is defined\n");
$closure.();
$OUT.print("ok 6 # smop lives\n");
my $closure2 = $p5interpreter.eval('
    sub {
        print "ok 7 # passing SV* values back and forth works\n" if $_[0] eq "abc" && $_[1] eq "123";
    }
');
$closure2.($p5interpreter.eval('"abc"'),$p5interpreter.eval('"123"'));


