#!/usr/bin/perl

# Simple YAML test harness written over Test::Harness::Straps.
# Hacked up from mini_harness.plx in the Test::Harness dist.

# Please improve me!
#
# TODO:
# 1. progress indication - can't stream YAML output because
#    YAML.pm doesn't support it yet. So at least let the user
#    know something's happening?
# 2. Modularize this.
# 3. Get output from Test::diag() in here somehow
# 4. Optionally look back at output from tests that passed and
#    make them more terse
# 5. Get to work concurrently with 'make test'
# 6. 'make smoke' make target that uploads the results of this
#    to a server somewhere.


package Test::Harness::YAML;
use Getopt::Long;
use YAML;
use Test::Harness;
use Test::Harness::Straps;
@ISA = qw(Test::Harness::Straps);

$| = 1;

GetOptions \our %Config, qw(-v);
$Test::Harness::Verbose = 1 if $Config{v};

my $s = __PACKAGE__->new;

sub log_event {
	my($self) = @_;
	push @{ $self->{_log} }, my $event = {};
	return $event;
}

sub latest_event {
	my($self) = @_;
	$self->{_log}[-1];
}

sub emit {
	my($self) = @_;
	print YAML::Dump($self->{_cases});
}

%handlers = (
    bailout     => sub {
        my($self, $line, $type, $totals) = @_;

		%{ $self->log_event } = (
			type => 'bailout',
			($self->{bailout_reason} ?
			 (reason => $self->{bailout_reason}) : ()),
		);
		$self->emit;
		exit 1;
    },
    test        => sub {
        my($self, $line, $type, $totals) = @_;
        my $curr = $totals->{seen};

		%{ $self->log_event } = (
			type   => 'test',
			num    => $curr,
			result => $totals->{details}[-1]{ok} ?
				"ok $curr/$totals->{max}" : "NOK $curr",
			($Test::Harness::Verbose ?
				(line => $line) : ()),
		);

        if( $curr > $self->{'next'} ) {
			$self->latest_event->{note} =
				"Test output counter mismatch [test $curr]\n";
        }
        elsif( $curr < $self->{'next'} ) {
            $self->latest_event->{note} =
				("Confused test output: test $curr answered after ".
                          "test ", $self->{next} - 1, "\n");
#            $self->{'next'} = $curr;
        }
    },
);

$s->{callback} = sub {
    my($self, $line, $type, $totals) = @_;

    $handlers{$type}->($self, $line, $type, $totals) if $handlers{$type};
};

my $width = Test::Harness::_leader_width(@ARGV);
foreach my $file (@ARGV) {
    ($leader, $ml) = Test::Harness::_mk_leader($file, $width);

    push @{ $s->{_cases} } , {
		file => $file,
		subtests => ($s->{_log} = []),
	};
    my %result = $s->analyze_file($file);
	$s->{_cases}[-1]{result} = $result{passing} ? 'ok' : 'FAILED';
}
$s->emit;

