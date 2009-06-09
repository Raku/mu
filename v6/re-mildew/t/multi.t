say "1..4";
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
    # so we can do if $.type {...} in the Param.ACCEPTS
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
