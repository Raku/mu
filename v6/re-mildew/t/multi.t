say "1..9";
{
my multi foo($arg1 is ref) {
    say $arg1;
}
my multi foo($arg1,$arg2) {
    say "ok 2";
}
foo("ok 1");
foo(1,2);
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
my multi foo($arg1,int $arg2) {
    say "ok 3";
}
my multi foo(int $arg1,$arg2) {
    say "ok 4";
}
foo("foo",1);
foo(1,"foo");
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
my multi foo(Foo $foo) {
    say "ok 5";
}
my multi foo(Bar $bar) {
    say "ok 6";
}
my $foo = Foo.new;
my $bar = Bar.new;
foo($foo);
foo($bar);
}.();

{
my multi foo($a) {
    say "ok 7 # calling the inherited candidate";
}
{
    my multi foo($a,$b) {
        say "ok 8 # one can locally add candidates";
    }
    foo(1);
    foo(1,2);
}.();
foo(1,2);
CATCH {
    say "ok 9 # the lexically added candidates are not visible in outer scopes";
}
}.();
