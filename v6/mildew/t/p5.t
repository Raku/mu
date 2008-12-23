$OUT.print("1..1\n");
my $p5interpreter = ::P5Interpreter.new();
$p5interpreter.eval("\$|=1;print 'ok 1\n'");
my $foo = $p5interpreter.eval("'ok 2\n'");
$OUT.print($foo.Str);
