#!/usr/bin/pugs
#Simple Markov chain Dissociated Press implementation
#adapted from http://cm.bell-labs.com/cm/cs/tpop/markov.pl

my $n = 2;
my $m = 10000;
my %s;
my $o = 0;
my @w = "\n" xx $n;			# initial sate

for $*ARGS.comb {			# read each word of input
	%s{[;] @w}.push: $_;
	@w.push: $_; @w.shift;		# advance chain
}
%s{[;] @w}.push: "\n";			# add tail

@w »=» "\n";
while ($_ = %s{[;] @w}.pick).say {
	last if /\n/ or $o++ >= $m;
	@w.push: $_; @w.shift;		# advance chain
}
