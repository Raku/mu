my $a = 0;
my $still_running = 2;
my $thr1 = async {
    say "Thread 1 started";
    contend {
        $a > 5 or defer;
        $a = -1000;
        $still_running--;
    }
    say 'Thread 1 finished: $a is >5 and reset to -1000';
}

my $thr2 = async {
    say "Thread 2 started";
    contend {
        maybe { $a > 100 or defer }
	maybe { $a < -100 or defer }
        $still_running--;
    }
    say 'Thread 2 finished: $a is now < -100'
}
while ($still_running) {
    say $a;
    sleep 1;
    contend { $a++; }
}
$thr1.join;
$thr2.join;
