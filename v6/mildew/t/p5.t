$OUT.print("1..6\n");
my $p5interpreter = ::P5Interpreter.new();
$p5interpreter.eval("local \$|=1;print 'ok 1\n'");
my $foo = $p5interpreter.eval("'ok 2\n';");
$OUT.print($foo.Str);
my $bar = $p5interpreter.eval("\$|=1;print 'ok 3\n';sub \{local \$|=1;print 'ok 5 # from p5 sub\n'\}");
$OUT.print("ok 4 # smop lives after p5 sub is defined\n");
$bar.();
$OUT.print("ok 6 # smop lives\n");

