use v6;
require Test;

plan(12);

my $pair = 'foo' => 'bar';
ok (ref $pair eq 'Pair');

my $foo = $pair.key;
my $bar = $pair.value;

ok ($foo eq 'foo');
ok ($bar eq 'bar');

my @pair1 = $pair.kv;

ok (@pair1[0] eq 'foo');
ok (@pair1[1] eq 'bar');

my $quux = eval '(quux => "xyzzy").key';

todo_ok ($quux eq 'quux', "lhs quotes" );

#Pair with a numeric value
my $pair = 'foo' => 2;
ok (ref $pair eq 'Pair');

my $two = $pair.value;
ok ($two == 2);

#Pair with a Pair value
my $pair = "foo" => ("bar" => "baz");
ok (ref $pair eq 'Pair');
my $pair2 = $pair.value;
ok (ref $pair2 eq 'Pair');

#Pair with a Pair key
$pair = ("foo" => "bar") => "baz";
ok (ref $pair eq 'Pair');
my $key = $pair.key;
ok (ref $key eq 'Pair');

#Pair list a la http://www.nntp.perl.org/group/perl.perl6.language/19360
my $list = 1 => 2 => 3 => 4;
