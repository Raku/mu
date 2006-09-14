package Test::Harness::YAML;
use strict;
use warnings;

use Benchmark;
use Best 0.05 [ [ qw/YAML::Syck YAML/], qw/LoadFile DumpFile/ ];
use File::Spec;
use Getopt::Long;
use List::Util 'shuffle';
use Test::Harness;
use Test::TAP::Model;

# Package and global declarations
our @ISA = qw(Test::TAP::Model);
our $SMOKERFILE = ".smoker.yml";
our %Config;
$ENV{TEST_ALWAYS_CALLER} = 1;
$Test::Harness::Verbose  = 1;

$| = 1;

sub get_config {
    GetOptions \%Config, qw(
        --concurrent|j=i    --shuffle|s   --exclude|X=s@
        --output-file|o=s   --recurse|r   --anonymous|a
        --include=s@        --dry|n       --help|h
    );
    fix_config();
    my $Usage = qq{Usage: $0 [OPTIONS] 
        --help, -h              This help message.
        --output-file=FILE, -o  Store results in FILE [default: $Config{"output-file"}]
        --dry, -n               Show which tests would be run but don't run them
        --concurrent=N, -j      Run N test jobs concurrently (MSWin requires Paralle::ForkManager)
        --shuffle, -s           Run tests in random order
        --recurse, -r           Recurse into directories on test include list
        --incude=I1,[I2,...]    Include files
        --exclude=E1,[E2,...]   Exclude files
        --anonymous, -a         Do not include ~/.smoker.yml data in report
    } . "\n";
    die $Usage if $Config{help};
}

sub fix_config {
    $Config{"concurrent"} ||= $ENV{PUGS_TESTS_CONCURRENT} || 1;
    local $@;
    eval { require Parallel::ForkManager; };
    if ($@) {
        if ($Config{"concurrent"} > 1 && $^O =~ /MSWin32|msys/) { # On cygwin we are okay.
            warn "Sorry, concurrency not supported on your platform\n";
            $Config{"concurrent"} = 1;
        }
        require POSIX;
    }
    else {
        no warnings 'redefine';
        *run_children = sub {
            my ($self, $child_count, $all_tests) = @_;
            my $pm =  Parallel::ForkManager->new($child_count);
            my $chunk_size = POSIX::ceil(@$all_tests / $child_count);

            for my $child (1 .. $child_count) {
                my @own_tests = splice @$all_tests, 0, $chunk_size;
                my $pid = $pm->start and next;
                # Inside child process now
                $self->{_child_num} = $child;
                $self->run_test($_) for @own_tests;
                $self->emit_chunk();
                $pm->finish;
                # Back in parent process now
                push @{ $self->{_children} }, $pid;
            }
            $self->gather_results();
        }
    }
    $Config{"output-file"} ||= "tests.yml";
    $Config{"recurse"}       = 1 if not defined $Config{"recurse"};
    # Needed for smokeserv
    $Config{"pugs-path"} = $ENV{HARNESS_PERL};
    push @{$Config{"exclude"}}, 'Disabled' if not $Config{"exclude"} or not @{$Config{"exclude"}};
    _build_include_re();
    _build_exclude_re();
}

get_config();

@ARGV = sort map glob, "t/*/*.t", "t/*/*/*.t", "ext/*/t/*.t" unless @ARGV;

my $s = __PACKAGE__->new;
$s->run;
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
            next if $Config{exclude_re} && $currfile =~ $Config{exclude_re};

            if ( -d $currfile ) {
                push( @hits, all_in( $currfile ) ) if $Config{recurse};
            } else {
                push( @hits, $currfile ) if $currfile =~ $Config{include_re};
            }
        }
    } else {
        warn "$start: $!\n";
    }

    return @hits;
}


# concurrency temp-file. FIXME: use a real temp file?
sub emit_chunk {
    my($self) = @_;
    DumpFile("tests.$$.yml", $self->structure);
}

sub emit {
    my($self) = @_;
    $self->{_timing}{end} = time;
    $self->{_timing}{duration} =
        $self->{_timing}{end} - $self->{_timing}{start};
    DumpFile($Config{"output-file"}, {
            meat => $self->structure,
            map { $_ => $self->{"_$_"} } qw{
                build_info smoker config revision timing
        }});
}

sub set_build_info {
    my($self) = @_;
    my $executable = $ENV{HARNESS_PERL} || "pugs";
    $ENV{PERL6LIB} = 'blib6/lib';
    $self->{_build_info} = join '', qx{$executable -V};
}

sub _build_exclude_re {
    my $excl = join "|", # map { quotemeta }
        map { split /,/ } @{ $Config{exclude} };
    $Config{exclude_re} = qr/($excl)/ if $excl;
}

sub _build_include_re {
    my @include = map { split /,/ } @{ $Config{include} };
    s/^\.// for @include;
    @include = ("t") unless @include;
    my $include_re = join( "|", map { quotemeta } @include );
    $Config{include_re} = qr/\.($include_re)$/;
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
        $self->{_smoker} = eval { LoadFile($SMOKERFILE) } ||
            eval { LoadFile(($ENV{HOME}||'')."/$SMOKERFILE") };
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

    @tests = grep { $_ !~ $Config{exclude_re} } @tests if $Config{exclude_re};

    if ( @tests ) {
        if ($Config{shuffle}) {
            @tests = shuffle(@tests);
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
    my($self) = @_;
    my $rev_get_cmd = $Config{"pugs-path"}.' -V:pugs_revision';
    do { $self->{_revision} = $1 if /pugs_revision: (\d+)\r?$/ } for `@{[$rev_get_cmd]}`;
    $self->{_revision} ||= "unknown";
    print "$rev_get_cmd returns revision '@{[$self->{_revision}]}'\n";
}

sub run {
    my $self = shift;
    return $self->SUPER::run(@_) if $Config{concurrent} == 1;
    my @tests = $self->get_tests;
    $self->run_children($Config{concurrent}, \@tests);
}

sub run_children {
    my ($self, $child_count, $all_tests) = @_;
    my $chunk_size = POSIX::ceil(@$all_tests / ($child_count * 3 - 1));
    for my $child (1 .. $child_count) {
        my $this_size = $chunk_size * 3;

        # Heuristic: Most of the first tests (ext/) are slow,
        # so we arbitrarily lower the first chunk by 1/3.
        $this_size -= $chunk_size if $child == 1;

        my @own_tests = splice @$all_tests, 0, $this_size;
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
        my $chunk = LoadFile($file) or die "can't parse chunk ($file)";
        push @{ $self->{meat}{test_files} }, @{$chunk->{test_files}};
        unlink $file or die "unlink: $!";
    }
    warn "all chunks completed.\n";
}

sub run_test {
    my $self = shift;
    my $test = shift;
    my @rest = @_;
    my $kid  = $self->{_child_num} ? "[$self->{_child_num}] " : "";
    warn "$kid$test\n";
    my $t = timeit( 1, sub {
        $self->SUPER::run_test($test, @rest);
    } );
    warn "    ".timestr($t)."\n";
}


__END__
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
