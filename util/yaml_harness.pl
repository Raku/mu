#!/usr/bin/perl

# Simple YAML test harness written over Test::Harness::Straps.
# Hacked up from mini_harness.plx in the Test::Harness dist.
# (And some stuff stolen from prove, too.)

# Please improve me!
#
# TODO:
# 1. Modularize this.
# 2. Get to work concurrently with 'make test'
# 3. 'make smoke' make target that uploads the results of this
#    to a server somewhere.


package Test::Harness::YAML;
use strict;

use Getopt::Long;
use YAML;
use Test::Harness;
use Test::Harness::Straps;
use File::Spec;
our @ISA = qw(Test::Harness::Straps);
our $SMOKERFILE = ".smoker.yml";

$| = 1;

GetOptions \our %Config, qw(--output-file|o=s --dry|n
		--shuffle|s --recurse|r --ext=s@ --anonymous|a --exclude|X=s@);
$Test::Harness::Verbose = 1;
$Config{"output-file"} ||= "tests.yml"
$Config{"recurse"} = 1 if not defined $Config{"recurse"};
$Config{"exclude"} = 'Disabled' if not defined $Config{"exclude"};
@ARGV = "t/" if !@ARGV;

_build_ext_re();
_build_exclude_re();


my $s = __PACKAGE__->new;


my %handlers = (
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
        my $curr = $totals->{seen}||0;
		#$self->{'next'} ||= 0;

		%{ $self->log_event } = (
			type   => 'test',
			num    => $curr,
			result => $totals->{details}[-1]{ok} ?
				"ok $curr/$totals->{max}" : "NOK $curr",
			line   => $line,
		);
		$self->latest_event->{todo} = 1 if $line =~ /# TODO/;

        if( $curr > $self->{'next'} ) {
			$self->latest_event->{note} =
				"Test output counter mismatch [test $curr]\n";
        }
        elsif( $curr < $self->{'next'} ) {
            $self->latest_event->{note} =
				("Confused test output: test $curr answered after ".
                          "test ", ($self->{'next'}||0) - 1, "\n");
#            $self->{'next'} = $curr;
        }
    },
	other      => sub {
		my($self, $line, $type, $totals) = @_;

		# XXX: we're missing some context, so we *assume* we were
		# called after a test. :/
		$self->latest_event->{diag} .= $line;
	 },
);

$s->{callback} = sub {
    my($self, $line, $type, $totals) = @_;

    $handlers{$type}->($self, $line, $type, $totals) if $handlers{$type};
};


foreach my $file (@{ $s->get_tests }) {
    push @{ $s->{_test_cases} }, {
		file => $file,
		subtests => ($s->{_log} = []),
	};
	warn "$file\n";
    my %result = $s->analyze_file($file);
	$s->{_test_cases}[-1]{result} = $result{passing} ? 'ok' : 'FAILED';
}
$s->emit;
exit 0;


sub all_in {
    my $start = shift;

    my @hits = ();

    local *DH;
    if ( opendir( DH, $start ) ) {
        while ( my $file = readdir DH ) {
            next if $file eq File::Spec->updir || $file eq File::Spec->curdir;
            next if $file eq ".svn";
            next if $file eq "CVS";
			next if $Config{exclude_re} && $file =~ $Config{exclude_re};

            my $currfile = File::Spec->catfile( $start, $file );
            if ( -d $currfile ) {
                push( @hits, all_in( $currfile ) ) if $Config{recurse};
            } else {
                push( @hits, $currfile ) if $currfile =~ $Config{ext_regex};
            }
        }
    } else {
        warn "$start: $!\n";
    }

    return @hits;
}

sub shuffle {
    # Fisher-Yates shuffle
    my $i = @_;
    while ($i) {
        my $j = rand $i--;
        @_[$i, $j] = @_[$j, $i];
    }
}

sub log_event {
	my($self) = @_;
	push @{ $self->{_log} }, my $event = {};
	return $event;
}

sub latest_event {
	my($self) = @_;
	$self->{_log}[-1] || $self->log_event;
}

sub emit {
	my($self) = @_;
	YAML::DumpFile($Config{"output-file"}, {
			map { $_ => $self->{"_$_"} } qw{
				build_info test_cases start_time smoker config revision
		}});
}

sub set_build_info {
	my($self) = @_;
	my $executable = $ENV{HARNESS_PERL} || "pugs";
	$self->{_build_info} = join '', qx{$executable -V};
}

sub _build_exclude_re {
	my $excl = join "|", map { quotemeta }
		map { split /,/ } @{ $Config{exclude} };
	$Config{exclude_re} = qr/($excl)/ if $excl;
}

sub _build_ext_re {
	my @ext = map { split /,/ } @{ $Config{ext} };
	s/^\.// foreach @ext;
	@ext = ("t") unless @ext;
	my $ext_regex = join( "|", map { quotemeta } @ext );
	$Config{ext_regex} = qr/\.($ext_regex)$/;
}

sub _init {
	my($self) = @_;
	$self->set_build_info;
	$self->get_smoker;
	$self->get_revision;
	$self->{_start_time} = time;

	# XXX: should i just include \%Config here?
	$self->{_config} = { shuffle => $Config{shuffle}+=0 };
}

sub get_smoker {
	my($self) = @_;
	if (!$Config{anonymous}) {
		$self->{_smoker} = eval { YAML::LoadFile($SMOKERFILE) } ||
			eval { YAML::LoadFile(($ENV{HOME}||'')."/$SMOKERFILE") };
		if (!$self->{_smoker}) {
			warn<<"AD";
Smoker info not found. Please create a file named $SMOKERFILE
either in this directory or under your home. You can use the
skeleton in util/smoker-example. Alternatively, say "--anonymous"
on the command line to withold your identity (and this message).
AD
		}
	}
	#$self->{_smoker} ||= { name => "anonymous" };
}

sub get_tests {
	my($self) = @_;
	my @tests;
	@ARGV = File::Spec->curdir unless @ARGV;
	push( @tests, -d $_ ? all_in( $_ ) : $_ ) for @ARGV;

	if ( @tests ) {
		shuffle(@tests) if $Config{shuffle};
		if ( $Config{dry} ) {
			print join( "\n", @tests, "" );
			exit 0;
		} else {
			print "# ", scalar @tests, " tests to run\n" if $Test::Harness::debug;
		}
	}
	$self->{_config}{test_count} = scalar @tests;
	\@tests;
}

sub get_revision {
	# TODO: generalize to non-svn trees
	my($self) = @_;
	do { $self->{_revision} = $1 if /Revision: (\d+)$/ } for `svn info`;
	$self->{_revision} ||= "unknown";
}
