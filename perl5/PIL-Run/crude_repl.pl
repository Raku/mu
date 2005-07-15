#!/usr/bin/perl -w
# run me...
# perl -Ilib -w crude_repl.pl
# with pugs in your PATH.

use PIL::Run::MainX;
use PIL::Run::EvalX;

sub p6_repl_simple {
    my $verbose = 0;
    while (1) {
	print "> ";
	my $line = readline STDIN;
	last if !defined $line;
	my @res = p6_eval($line);
	print "\n",@res,"\n";
    }
}

sub p6_repl {
    my $verbose = 0;
    print ":v  toggles verbose output\n";
    print "say 'hi' and say 3 are about all that works.\n";
    while (1) {
	print "p5ugs> ";
	my $line = readline STDIN;
	last if !defined $line;
	if ($line =~ /\A\s*:v\s*\Z/) {
	    $verbose = !$verbose;
	    next;
	}
	my $p6 = $line;
	my $pil = pil_from_p6($p6);
	print $pil,"\n" if $verbose;
	my $pilc = pilc_from_pil($pil);
	print $pilc,"\n\n" if $verbose;
	my $p5r = p5r_from_pilc($pilc);
	print $p5r,"\n" if $verbose;
	print "----\n";
	my @res = run_p5r($p5r);
	print "\n",@res,"\n";
    }
}

p6_repl();

__END__
