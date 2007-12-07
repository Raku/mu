grammar Test {
    token foo  {1|abc};
    token foox3  {x<foo><foo><foo>};
};
say "1..5";
$_ = '2';
if (Test.foo()) {
    say "not ok 1";
} else {
    say "ok 1";
}
$_ = '1';
if (Test.foo()) {
    say "ok 2";
} else {
    say "not ok 2";
}
$_ = 'abc';
if (Test.foo()) {
    say "ok 3";
} else {
    say "not ok 3";
}
$_ = 'xabc1abc';
if (Test.foox3()) {
    say "ok 4";
} else {
    say "not ok 4";
}
$_ = 'xabcabc';
if (Test.foox3()) {
    say "not ok 5";
} else {
    say "ok 5";
}
