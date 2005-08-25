#!/usr/bin/pugs

use v6;
use Test;

=kwid

Bits and Pieces.

=cut

plan 17;

my $str;
$str = "hello";
ok($str eq 'hello', "basic quote");
$str = qq[world];
ok($str eq 'world', "qq bracket");
$str = qq{hello};
ok($str eq 'hello', "qq brace");
$str = qq<world>;
ok($str eq 'world', "qq angle");
$str = qq>hello<;
ok($str eq 'hello', "qq backwards angle");
$str = qq/world/;
ok($str eq 'world', "qq slash");

my $hello = "Hello";
$str = "$hello, World";
ok($str eq 'Hello, World', "dq interpolation");
$str = "Dear World, $hello";
ok($str eq 'Dear World, Hello', "dq ultimate interpolation");
$str = "I say $hello, World";
ok($str eq 'I say Hello, World', "dq internal interpolation");
$str = "$hello, World, I say $hello";
ok($str eq 'Hello, World, I say Hello', "foghorn leghorn interpolation");

my @array;
@array = qw/"foo" "bar"/;
ok(@array[0] eq '"foo"' and @array[1] eq '"bar"', 'qw//');

my @array;
@array = q:w/"foo" "bar"/;
ok(@array[0] eq '"foo"' and @array[1] eq '"bar"', 'q:w//');

my %hash;
%hash<Mon Tue Wed Thu Fri Sat Sun> = 1..7;;
ok(%hash{'Mon'} eq '1' and %hash{'Sun'} eq '7', '%hash<>');

if ($*OS eq "browser") {
    skip_rest "Programs running in browsers don't have access to regular IO.";
    exit;
}

sub nonce () { return (".$*PID." ~ int rand 1000) }
my $filename = "tmpfile" ~ nonce;

my $out = open("$filename", :w);
$out.say("line1");
$out.say("line2");
$out.close;

my $in0 = open("$filename");
my $line = $in0.readline;
is($line, "line1\n", "file I/O (scalar, readline)");
$in0.close;

my $in1 = open("$filename");
my $line = =$in1;
is($line, "line1\n", "file I/O (scalar, unary =)");
$in1.close;

my $in2 = open("$filename");
my @line = =$in2;
is("@line[]", "line1\n line2\n", "file I/O (list, unary =)");
$in2.close;

ok(unlink("$filename"), "file unlinked");
