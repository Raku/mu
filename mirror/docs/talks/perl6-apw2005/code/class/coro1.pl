#=Coroutinen
use v6;
coro zähler {
	for 1..10 -> $i {
		yield $i;
	}
}

my $i;
while $i = zähler() != 0 {
	say $i;
}
