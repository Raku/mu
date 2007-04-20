# This test script serves as a regression test suite harness
# for util/smartlinks.pl
# Because smartlinks.pl depends on many changing things like
# the Pugs test suite and Perl 6 Synopses, we have to freeze
# the dependencies in order to do an effiective regression.
# To save the room in the SVN repos, this script downloads
# a tarball from feather itself, which contains a particular version
# of the test suite and Synopses, as well as the reference outputs
# in HTML.
#
# Dependencies:
#   LWP::UserAgent
#   Archive::Tar
#   Text::Diff

use strict;
use warnings;

#use Smart::Comments;
use Test::More tests => 20;
use File::Slurp;
use POSIX;
use FindBin qw( $Bin );
use File::Copy;

BEGIN {
    eval "use LWP::UserAgent;";
    if ($@) { die "LWP::UserAgent is required to run this test script"; }
    eval "use Archive::Tar;";
    if ($@) { die "Archive::Tar is required to run this test script"; }
    eval "use Text::Diff;";
    if ($@) { die "Text::Diff is required to run this test script"; }
};

my $tmpfile = POSIX::tmpnam();

my $data_path = "$Bin/smartlinks_data";
### $data_path;
if (-d $data_path) {
    warn "warning: Removing the exising $data_path...\n";
    system("$^X -MExtUtils::Command -e rm_rf $data_path");
}

my $ua = LWP::UserAgent->new;
my $url = 'http://feather.perl6.nl/~agentzh/smartlinks_data.tar.gz';
warn "info: Updating $url...\n";
my $tarball = "$Bin/smartlinks_data.tar.gz";
my $res = $ua->mirror($url, $tarball);
if ($res->is_error) {
    die "Failed to get $url: ", $res->status_line;
}
ok -f $tarball, 'tarball downloaded okay';

warn "info: Extracting $tarball...\n";
my $tar = Archive::Tar->new;
$tar->read($tarball);
chdir $Bin;
$tar->extract;

ok -d $data_path, 'tarball extracted successfully';
ok -d "$data_path/Spec", 'Spec/ okay';
ok -d "$data_path/expected", 'expected/ okay';
ok -d "$data_path/t", 't/ okay';

chdir $data_path;
system("$^X $Bin/../smartlinks.pl --fast " .
    "--syn-dir Spec --out-dir got " .
    "--dir t");
is $? >> 8, 0, "smartlinks.pl returned zero status";

for my $path (glob "expected/*.html") {
    my $file;
    if ($path =~ m{/([^/]+)$}) {
        $file = $1;
    }
    fix_randomness("got/$file");
    fix_randomness("expected/$file");
    my $cmd = "diff got/$file expected/$file";
    my ($file1, $file2) = ("got/$file", "expected/$file");
    warn "$file1 <=> $file2\n";
    my $out = diff $file1, $file2, {STYLE => 'OldStyle'};
    #$out =~ s/70c70\n<[^\n]*\n---\n>[^\n]*\n//;
    #$out =~ s/82c82\n<[^\n]*\n---\n>[^\n]*\n//;
    #$out =~ s/82,83c82,83\n(?:<[^\n]*\n){2}---\n(?:>[^\n]*){2}\n//;
    #$out =~ s/9c9\n<[^\n]*\n---\n>[^\n]*\n//;
    #$out =~ s/(?x) 50,52c50,52 \n (?: < [^\n]* \n){3} --- \n (?: > [^\n]* \n){3}//;
    #$out =~ s/64,65c64,65\n<[^\n]*\n<[^\n]*\n---\n>[^\n]*\n>[^\n]*\n//;
    $out =~ s/^\s+|\s+$//g;
    is $out, '', 'no diff';
}

sub fix_randomness {
    my $fname = shift;
    open my $in, $fname or die "Can't open $fname for reading: $!";
    open my $out, "> $tmpfile" or die "Can't write to $tmpfile: $!";
    while (<$in>) {
        s/\br\d+\b/rXXXX/g;
        s/\d{4}-\d{2}-\d{2} \d+:\d+:\d+ GMT/XXXX-XX-XX XX:XX:XX GMT/g;
        s/\bhide_\d+\b/hide_XX/g;
        s/\bheader_hidden_\d+\b/header_hidden_XX/g;
        s/\bheader_shown_\d+\b/header_shown_XX/g;
        s/\bPerl v5\.\d+\b at [^\n<>]+/Perl v5.XXX at XXX./g;
        print $out $_;
    }
    close $out;
    close $in;
    #system("sudo cp $tmpfile $fname");
    copy $tmpfile, $fname;
    unlink $tmpfile;
}

