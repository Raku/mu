use v6;

=pod

This is a test file.  Whee!

=cut
say "1..6";

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

sub ok ($var, $loop) {
    if ($var) {
	say("ok ", $loop, " # TODO var = ", $var);
    }
    else {
	say("not ok ", $loop, " # TODO");
    }
}


eval '$bar = $::($foo)';
ok ($bar, 1);
$bar = '';
eval '$bar = $::("MY::$foo")';
ok ($bar, 2);
$bar = '';
eval '$bar = $::($foobar)';
ok ($bar, 3);
$bar = undef;
eval ' $bar = %MY::<$foo> ';
ok ($bar, 4);

my @array;
eval ' @array = qw/"foo" "bar"/ ';
if (@array) { say 'ok 5' } else { say 'not ok 5 # TODO' }

my @array;
eval ' @array = q:w/"foo" "bar"/ ';
if (@array) { say 'ok 6' } else { say 'not ok 6 # TODO' }

my %hash;
eval ' %hash<Mon Tue Wed Thu Fri Sat Sun> = 1..7; ';
if (%hash) { say 'ok 7' } else { say 'not ok 7 # TODO' }
