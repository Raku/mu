#!/usr/bin/pugs

use v6;
use Test;

plan 2;

{
	my $i = 0;
	while (defined($i)) { if (++$i < 3) { redo }; last }
	is($i, 3, "redo caused reapplication of block");
}

{
	my @log;	
	my $i;
	while ++$i < 5 {
		push @log, "before";
		if (++$i == 2){
			redo;
		} else {
			push @log, "no_redo";
		}
		push @log, "after";
	}
	
	is(~@log, "before before no_redo after before no_redo after", "statements after redo are not executed", :todo<bug>);
}

