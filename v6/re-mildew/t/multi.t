say "1..6";
{
my $multi = ::Multi.new;
$multi.candidates.push(sub ($arg1 is ref) {
    say $arg1;
});
$multi.candidates.push(sub ($arg1,$arg2) {
    say "ok 2";
});
$multi.("ok 1");
$multi.(1,2);
}.();

{
role int {
    method ACCEPTS($thing) {
        PRIMITIVES::ritest((|$thing),PRIMITIVES::SMOP_RI(2));
    }
    # HACK so we can do if $.type {...} in the Param.ACCEPTS
    method true {
        ::True;
    }
}
my $multi = ::Multi.new;
$multi.candidates.push(sub ($arg1,int $arg2) {
    say "ok 3";
});
$multi.candidates.push(sub (int $arg1,$arg2) {
    say "ok 4";
});
$multi.("foo",1);
$multi.(1,"foo");
}.();

{
role Foo {
    # HACK so we can do if $.type {...} in the Param.ACCEPTS
    method true {
        ::True;
    }
}
role Bar {
    # HACK so we can do if $.type {...} in the Param.ACCEPTS
    method true {
        ::True;
    }
}
my $multi = ::Multi.new;
$multi.candidates.push(sub (Foo $foo) {
    say "ok 5";
});
$multi.candidates.push(sub (Bar $bar) {
    say "ok 6";
});
my $foo = Foo.new;
my $bar = Bar.new;
$multi.($foo);
$multi.($bar);
}.();
