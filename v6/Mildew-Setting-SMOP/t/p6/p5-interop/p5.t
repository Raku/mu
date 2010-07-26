say "1..13";
EXTERNAL::eval_perl5('$| = 1');
$OUT.unbuffered;

EXTERNAL::eval_perl5('print "ok 1\n"');
my $foo = EXTERNAL::eval_perl5('"ok 2";');
say $foo.Str;
my $closure = EXTERNAL::eval_perl5('print "ok 3\n";sub {print "ok 5 # from p5 sub\n"}');
say "ok 4 # smop lives after p5 sub is defined";
$closure();
say "ok 6 # smop lives";
my $closure2 = EXTERNAL::eval_perl5('
    sub {
        print "ok 7 # passing SV* values back and forth works\n" if $_[0] eq "abc" && $_[1] eq "123";
    }
');
$closure2(EXTERNAL::eval_perl5('"abc"'),EXTERNAL::eval_perl5('"123"'));
my $p5object = EXTERNAL::eval_perl5('
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
$p5object.ok8(EXTERNAL::eval_perl5('"some StRiNg"'));
say "ok ",EXTERNAL::eval_perl5('9').int," # int works";

knowhow Foo {
    method ok10 {
        say "ok 10";
    }
    method ok12 {
        say "ok 12";
    }
}
(EXTERNAL::eval_perl5('sub {$::smop_object = $_[0]}'))(Foo.FETCH);
EXTERNAL::eval_perl5('
    $::smop_object->ok10;
    print "ok 11\n";
    $::smop_object->ok12;
');
say "ok 13";
