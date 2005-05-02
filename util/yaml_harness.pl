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
use Test::TAP::Model;
use File::Spec;
our @ISA = qw(Test::TAP::Model);
our $SMOKERFILE = ".smoker.yml";

$| = 1;

$ENV{TEST_ALWAYS_CALLER} = 1;

GetOptions \our %Config, qw(--output-file|o=s --dry|n --concurrent|j=i
        --shuffle|s --recurse|r --ext=s@ --anonymous|a --exclude|X=s@);
fixup_concurrency();
$Test::Harness::Verbose  = 1;
$Config{"output-file"} ||= "tests.yml";
$Config{"recurse"} = 1 if not defined $Config{"recurse"};
push @{$Config{"exclude"}}, 'Disabled' if not $Config{"exclude"} or not @{$Config{"exclude"}};
@ARGV = "t/" if !@ARGV;

_build_ext_re();
_build_exclude_re();

my $s = __PACKAGE__->new;
$s->run;
$s->emit;
exit 0;


sub fixup_concurrency {
    $Config{"concurrent"} ||= $ENV{PUGS_TESTS_CONCURRENT} || 1;
    if ($^O =~ /MSWin32|msys/) { # On cygwin we are okay.
		warn "Sorry, concurrency not supported on your platform\n";
		$Config{"concurrent"} = 1;
		return;
	}
    require POSIX;
}

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

# concurrency temp-file. FIXME: use a real temp file?
sub emit_chunk {
    my($self) = @_;
    YAML::DumpFile("tests.$$.yml", $self->structure);
}

sub emit {
    my($self) = @_;
    $self->{_timing}{end} = time;
    $self->{_timing}{duration} =
        $self->{_timing}{end} - $self->{_timing}{start};
    YAML::DumpFile($Config{"output-file"}, {
            meat => $self->structure,
            map { $_ => $self->{"_$_"} } qw{
                build_info smoker config revision timing
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
    my($self) = shift;
    $self->set_build_info;
    $self->get_smoker;
    $self->get_revision;
    
	$Config{shuffle}+=0;
    $self->{_config} = \%Config;
    $self->{_timing}{start} = time;

    $self->SUPER::_init(@_);
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
        if ($Config{shuffle}) {
            shuffle(@tests)
        } else {
            # default FS order isn't guaranteed sorted; and sorting
            # helps diffing raw YAML results.
            @tests = sort @tests;
        }
        if ( $Config{dry} ) {
            print join( "\n", @tests, "" );
            exit 0;
        } else {
            print "# ", scalar @tests, " tests to run\n" if $Test::Harness::debug;
        }
    }
    $self->{_config}{test_count} = scalar @tests;
    @tests;
}

sub get_revision {
    # TODO: generalize to non-svn trees
    my($self) = @_;
    do { $self->{_revision} = $1 if /Revision: (\d+)$/ } for `svn info`;
    $self->{_revision} ||= "unknown";
}

sub run {
    my $self = shift;
    return $self->SUPER::run(@_) if $Config{concurrent} == 1;
    my @tests = $self->get_tests;
    $self->run_children($Config{concurrent}, \@tests);
}

sub run_children {
    my ($self, $child_count, $all_tests) = @_;
    my $chunk_size = POSIX::ceil(@$all_tests / $child_count);
    for my $child (1 .. $child_count) {
        my @own_tests = splice @$all_tests, 0, $chunk_size;
        defined(my $pid = fork) or die "Can't fork: $!";
        if ($pid) {
            push @{ $self->{_children} }, $pid;
        } else {
            $self->{_child_num} = $child;
            $self->run_test($_) for @own_tests;
            $self->emit_chunk();
            exit 0;
        }
    }
    $self->gather_results();
}

# the wait here is sequential rather than nonblocking / as-they-come, because
# we want to preserve ordering anyway and it's probably okay to keep a few
# zombies around for a relatively short while.
sub gather_results {
    my($self) = @_;
    my $kid;
    for my $pid (@{ $self->{_children} }) {
        my $file = "tests.$pid.yml";
        warn sprintf "waiting for chunk #%d...\n", ++$kid; 
        waitpid($pid, 0) or die "waitpid: $!";
        my $chunk = YAML::LoadFile($file) or die "can't parse chunk ($file)";
        push @{ $self->{meat}{test_files} }, @{$chunk->{test_files}};
        unlink $file or die "unlink: $!";
    }
	warn "all chunks completed.\n";
}

sub run_test {
    my $self = shift;
    my $test = shift;
    my $kid  = $self->{_child_num} ? "[$self->{_child_num}] " : "";
    warn "$kid$test\n";
    $self->SUPER::run_test($test, @_);
}
