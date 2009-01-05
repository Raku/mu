package VAST::variable;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{twigil} && $m->{twigil}[0] &&
	$m->{twigil}[0]{sym} &&
	$m->{twigil}[0]{sym} eq '!') {
	call('postcircumfix:{ }'=>
	     FETCH(call('postcircumfix:{ }',
			FETCH(call('^!instance_storage'=>
				   FETCH(lookup('$¿self')))),
		   [FETCH(call(name=>FETCH(lookup('$?PACKAGE'))))])),
             [string varname($m)])
    } elsif ($m->{twigil}[0]{sym} eq '.') {
	XXX;
    } else {
	AST::Call->new(
	    identifier=>string 'lookup',
	    capture=>AST::Capture->new(invocant=>reg '$scope',positional=>[string varname($m)]),
	    );
    }
}

1;
