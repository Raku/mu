#!/usr/bin/perl

# Simple YAML test harness written over Test::Harness::Straps.
# Hacked up from mini_harness.plx in the Test::Harness dist.
# (And some stuff stolen from prove, too.)

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
use strict;

use Getopt::Long;
use YAML;
use Test::Harness;
use Test::Harness::Straps;
use File::Spec;
our @ISA = qw(Test::Harness::Straps);

$| = 1;

GetOptions \our %Config, qw(--verbose|v --output-file|o=s --shuffle|s --recurse|r --ext=s@);
$Test::Harness::Verbose = 1 if $Config{verbose};
$Config{"output-file"} ||= "-";
_build_ext_re();


my $s = __PACKAGE__->new;

$s->set_version_info;

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


foreach my $file (@tests) {
    push @{ $s->{_cases} } , {
		file => $file,
		subtests => ($s->{_log} = []),
	};
    my %result = $s->analyze_file($file);
	$s->{_cases}[-1]{result} = $result{passing} ? 'ok' : 'FAILED';
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
	$self->{_log}[-1];
}

sub emit {
	my($self) = @_;
	YAML::DumpFile($Config{"output-file"},
		{
			build_info => $self->{_version_info},
			test_cases => $self->{_cases},
		});
}

sub set_version_info {
	my($self) = @_;
	my $executable = $ENV{HARNESS_PERL} || "pugs";
	$self->{_version_info} = join '', qx{$executable -V};
}

sub _build_ext_re {
# Build up extensions regex
	my @ext = map { split /,/ } @{ $Config{ext} };
	s/^\.// foreach @ext;
	@ext = ("t") unless @ext;
	my $ext_regex = join( "|", map { quotemeta } @ext );
	$Config{ext_regex} = qr/\.($ext_regex)$/;
}
