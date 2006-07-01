my $a = 0;
my $still_running = 2;
my $thr1 = async {
    say "Thread 1 started";
    sync {
        $a > 5 or resync;
        $a = -1000;
        $still_running--;
    }
    say 'Thread 1 finished: $a is >5 and reset to -1000';
}

my $thr2 = async {
    say "Thread 2 started";
    sync {
        maybe { $a > 100 or resync }
	maybe { $a < -100 or resync }
        $still_running--;
    }
    say 'Thread 2 finished: $a is now < -100'
}
while ($still_running) {
    say $a;
    sleep 1;
    sync { $a++; }
}
$thr1.join;
$thr2.join;
