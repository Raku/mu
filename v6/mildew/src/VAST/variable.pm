package VAST::variable;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    my $twigil = $m->{twigil}[0]{sym} || '';
    if ($twigil eq '!') {
	call('postcircumfix:{ }'=>
	     FETCH(call('postcircumfix:{ }',
			FETCH(call('^!instance_storage'=>
				   FETCH(lookup('$¿self')))),
		   [FETCH(call(name=>FETCH(lookup('$?PACKAGE'))))])),
             [string varname($m)])
    } elsif ($twigil eq '.') {
	call($m->{desigilname}{longname}->canonical,FETCH(lookup('$¿self')));
    } else {
	AST::Call->new(
	    identifier=>string 'lookup',
	    capture=>AST::Capture->new(invocant=>reg '$scope',positional=>[string varname($m)]),
	    );
    }
}

1;
