#!/usr/bin/pugs

use v6;
require Test;

=pod

This is a test file.  Whee!

=cut

my $foo = "Foo";
my $foobar = "Foo::Bar";
my $bar;

eval '$bar = $::($foo)';
todo_ok ($bar, 'symbolic deref');
$bar = '';
eval '$bar = $::("MY::$foo")';
todo_ok ($bar, 'symbolic deref on lexical scope');
$bar = '';
eval '$bar = $::($foobar)';
todo_ok ($bar, 'more symbolic deref');
$bar = undef;
eval ' $bar = %MY::<$foo> ';
todo_ok ($bar, 'hash deref on lexical scope');

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
todo_ok ($bar, '*FILE');
