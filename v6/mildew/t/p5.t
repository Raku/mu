say "1..13";
my $p5 = ::P5Interpreter.new();
 
$p5.eval('$| = 1');
$OUT.unbuffered;

$p5.eval('print "ok 1\n"');
my $foo = $p5.eval('"ok 2\n";');
say $foo.Str;
my $closure = $p5.eval('print "ok 3\n";sub {print "ok 5 # from p5 sub\n"}');
say "ok 4 # smop lives after p5 sub is defined";
$closure.();
say "ok 6 # smop lives";
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
say "ok ",$p5.eval('9').int," # int works";

knowhow Foo {
    method ok10 {
        say "ok 10";
    }
    method ok12 {
        say "ok 12";
    }
}
$p5.eval('sub {$::smop_object = $_[0]}').(Foo.FETCH);
$p5.eval('
    $::smop_object->ok10;
    print "ok 11\n";
    $::smop_object->ok12;
');
say "ok 13";
