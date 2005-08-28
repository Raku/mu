#!/usr/bin/perl

use strict;
use warnings;
use Shell qw(svn);
use Config;
use File::Spec;
use FindBin;

my $failed = 0;
for (qw/YAML Test::TAP::Model Test::TAP::HTMLMatrix/) {
    check_prereq($_) or $failed++;
}

die <<"EOF" if $failed;

You don't seem to have the required modules installed.
Please install it from the CPAN and try again.
EOF
#'

#
# run-smoke.pl /some/sandbox/dir /some/www/file.html
#
my $pugs_sandbox    = $ARGV[0] or die "Need pugs sandbox location";
my $html_location   = $ARGV[1] or die "Need HTML output file location";
my $optional_args   = $ARGV[2] || "";

chdir($pugs_sandbox) or die "Could change directory: $!";

$ENV{HARNESS_PERL}  = "./pugs";
$ENV{HARNESS_PERL}  = "$^X $FindBin::Bin/../perl5/PIL2JS/pugs-smokejs.pl ./pugs $optional_args"
    if $ENV{PUGS_RUNTIME} and $ENV{PUGS_RUNTIME} eq 'JS';
$ENV{HARNESS_PERL}  = "$^X $FindBin::Bin/../perl5/PIL-Run/crude_repl.pl"
    if $ENV{PUGS_RUNTIME} and $ENV{PUGS_RUNTIME} eq 'PERL5';

$ENV{PERL6LIB}      = join $Config{path_sep},
        qw<ext/Test/lib blib6/lib>, $ENV{PERL6LIB};

my @yaml_harness_args;
push(@yaml_harness_args,'--exclude','Disabled,^ext\b')
    if $ENV{PUGS_RUNTIME} and ($ENV{PUGS_RUNTIME} eq 'JS' or
			       $ENV{PUGS_RUNTIME} eq 'PERL5');

sub make { return `$Config{make} @_` };
my $dev_null = File::Spec->devnull;

my $output ;# = svn("up") or die "Could not update pugs tree: $!";
system($^X, qw(-w ./util/yaml_harness.pl),@yaml_harness_args) == 0 or die "Could not run yaml harness: $!";
system($^X, qw(-w ./util/testgraph.pl --inlinecss tests.yml), $html_location) == 0 or die "Could not convert .yml to testgraph: $!";
upload_smoke($html_location);
print "*** All done! Smoke matrix saved as '$html_location'.\n";

sub upload_smoke {
	my ($loc) = @_;
	return unless defined $ENV{PUGS_SMOKE_UPLOAD};
	system("$ENV{PUGS_SMOKE_UPLOAD} $loc") == 0 or die "couldn't run user smoke upload command: $!";
}

sub check_prereq {
    my ($mod) = @_;
    (my $file = $mod) =~ s,::,/,g;
    if (eval { require "$file.pm"; 1 }) {
        return 1;
    }
    else {
        warn "$mod - missing dependency\n";
        warn "($@)\n" if $@ and $@ !~ /Can't locate \Q$file\E/;
        return 0;
    }
}
# END
