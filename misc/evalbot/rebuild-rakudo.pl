use strict;
use warnings;
use File::Path qw(rmtree);
use 5.010;
use autodie;
use Data::Dumper;

my $home = glob('~') . '/';
my @dirs = qw(p1 p2);
my %swap = (@dirs, reverse(@dirs));

my $link = 'p';

my $now = readlink $link;
my $other = $swap{$now};

say "Other: '$other'";
chdir "${home}rakudo";
system('git', 'pull');

my $revision_file = "$home$other/rakudo-revision";
for ($revision_file) {
    open my $fh, '<', $revision_file or break;
    my $r = <$fh>;
    close $fh;

    chomp $r;
    my $needs_rebuild = `git rev-parse HEAD | grep ^$r|wc -l`;
    chomp $needs_rebuild;
    if ($needs_rebuild) {
        say "Don't need to rebuild, we are on the newest revision anyway";
        exit;
    }
}

my $revision = `cat build/PARROT_REVISION`;
if ($revision =~ m/^(\d+)/) {
    $revision = $1;
} else {
    die "Can't handle revision '$revision'";
}
say "Requiring revision $revision";

my $parrot_config = "$home$other/bin/parrot_config";
my $available = `$parrot_config revision`;
chomp $available;
say "Revision $available available";
if ($available <= $revision) {
    chdir 'parrot';
    system('make', 'distclean');
    system('svn', 'up', "-r$revision")                  and die $?;
    system($^X, 'Configure.pl', "--prefix=$home/$other",
                '--nomanicheck', '--cc=ccache gcc')     and die $?;
    system('make' )                              and die $?;
    system('make', 'install')                           and die $?;
    system('make', 'install-dev')                       and die $?;
    chdir "${home}rakudo";
}

system($^X, 'Configure.pl', "--parrot-config=$parrot_config");
system('make', 'Test.pir')      and die $?;
system('make', 'install')       and die $?;
system("git rev-parse HEAD | cut -b 1,2,3,4,5,6 > $revision_file") and warn $?;

=for comment

eval {
	chdir glob('~/blizkost/');
    system('make', 'clean');        # may fail, nor warning here
	system('git', 'pull')           and warn $?;
	print "$^X 'Configure.pl --parrot-config=$home$other/bin/parrot_config";
	
	system($^X, 'Configure.pl', "--parrot-config=$home$other/bin/parrot_config")
		and warn $?;
	system('make') 			and warn $?;
	system('make', 'install') 	and warn $?;
};

=cut

chdir $home;
unlink $link;
symlink $other, $link;

