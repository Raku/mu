use v6;

=pod

This is a test file.  Whee!

=cut

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;
my $loop = 0;

sub ok ($cond, Str $descr) {
    $loop++;

    if ($cond) {
	say("ok ", $loop, " # ", $descr, " (var = ", $cond, ")");
    }
    else {
	say("not ok ", $loop, " # TODO ", $descr);
    }
};


eval '$bar = $::($foo)';
ok ($bar, 'symbolic deref');
$bar = '';
eval '$bar = $::("MY::$foo")';
ok ($bar, 'symbolic deref on lexical scope');
$bar = '';
eval '$bar = $::($foobar)';
ok ($bar, 'more symbolic deref');
$bar = undef;
eval ' $bar = %MY::<$foo> ';
ok ($bar, 'hash deref on lexical scope');

my @array;
eval ' @array = qw/"foo" "bar"/ ';
ok(@array, 'qw//');

my @array;
eval ' @array = q:w/"foo" "bar"/ ';
ok(@array, 'q:w//');

my %hash;
eval ' %hash<Mon Tue Wed Thu Fri Sat Sun> = 1..7; ';
ok(%hash, '%hash<>');

eval '
    my $handle = open(">/tmp/tmpfile");
    for *FILE { $bar ~= $_ }
';
ok ($bar, '*FILE');

say "1..", $loop;
