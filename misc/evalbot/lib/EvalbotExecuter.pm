package EvalbotExecuter;

use strict;
use warnings;
use utf8;
use BSD::Resource;
use Carp qw(confess);
use File::Temp qw(tempfile);

my $max_output_len = 150;

sub run {
	my ($program, $executer) = @_;
	my $response = _fork_and_eval($program, $executer);
	my $newline = '␤';
	$response =~ s/\n/$newline/g;
	if (length $response > $max_output_len){
		$response = substr $response, 0, $max_output_len - 3;
		$response .= '...';
	}
	return "OUTPUT[$response]";
}

sub _fork_and_eval {
	my ($program, $executer) = @_;

	# the forked process should write its output to this tempfile:
	my ($fh, $filename) = tempfile();

	my $fork_val = fork;
	if (!defined $fork_val){
		confess "Can't fork(): $!";
	} elsif ($fork_val == 0) {
		_set_resource_limits();
		&$executer($program, $fh, $filename);
		exit;
	} else {
		# server
		wait;
	}

	# gather result
	close $fh;
	open ($fh, '<', $filename) or confess "Can't open temp file <$filename>: $!";
	my $result;
	{
		local $/;
		$result = join '', <$fh>;
	}
	unlink $filename;
	return $result;
}

sub _set_resource_limits {
	# stolen from evalhelper-p5.pl
	# 5s-7s CPU time, 100 MiB RAM, maximum of 500 bytes output.
	setrlimit RLIMIT_CPU,   15, 20                  or confess "Couldn't setrlimit: $!\n";
	setrlimit RLIMIT_VMEM,  80 * 2**20, 100 * 2**20 or confess "Couldn't setrlimit: $!\n";
	# PIL2JS writes to a tempfile.
	setrlimit RLIMIT_FSIZE, 50000, 50000,           or confess "Couldn't setrlimit: $!\n";
}

