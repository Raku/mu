$OUT.print("1..9\n");
my $p5 = ::P5Interpreter.new();

$p5.eval('$| = 1');
$OUT.unbuffered;

$p5.eval('print "ok 1\n"');
my $foo = $p5.eval('"ok 2\n";');
$OUT.print($foo.Str);
my $closure = $p5.eval('print "ok 3\n";sub {print "ok 5 # from p5 sub\n"}');
$OUT.print("ok 4 # smop lives after p5 sub is defined\n");
$closure.();
$OUT.print("ok 6 # smop lives\n");
my $closure2 = $p5.eval('
    sub {
        print "ok 7 # passing SV* values back and forth works\n" if $_[0] eq "abc" && $_[1] eq "123";
    }
');
$closure2.($p5.eval('"abc"'),$p5.eval('"123"'));
my $p5object = $p5.eval('
    package Foo;
    sub ok8 {
        my ($self,$arg) = @_;
        if ($self->{attr} == 175 && $arg eq "some StRiNg") {
            print "ok 8 # method call on SV* works\n";
        } else {
            print "not ok 8 #",$self->{attr},"|",$arg,"\n";
        }
    }
    bless {attr => 175},"Foo";
');
$p5object.ok8($p5.eval('"some StRiNg"'));
$OUT.print("ok ",$p5.eval('9').int," # int works\n");
