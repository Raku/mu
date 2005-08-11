#=temp1.pl - Anwendung von temp
my $log = 0; sub log { say if $log; }

sub beispiel {
	temp $log = 1;
	log "test2";
}

log "test1";
beispiel;
log "test3";
