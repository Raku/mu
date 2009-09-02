use strict;
use warnings;
use File::Path qw(rmtree);
use autodie;

my $home = glob('~') . '/';
my @dirs = qw(p1 p2);
my %swap = (@dirs, reverse(@dirs));

my $link = 'p';

my $now = readlink $link;
my $other = $swap{$now};
rmtree($other);
mkdir($other);
chdir('rakudo/parrot');
system('svn', 'up', '-q', '--force') and die $?;
system($^X, 'Configure.pl', "--prefix=$home$other", '--nomanicheck')
    and die $?;

system('make')                  and die $?;
system('make', 'install')       and die $?;
system('make', 'install-dev')   and die $?;

chdir "${home}rakudo";

my $parrot_config = "$home$other/bin/parrot_config";
die "No parrot_config at '$parrot_config'" unless -x $parrot_config;

system('git', 'pull') and die $?;
print join(' ', ($^X, 'Configure.pl',
            "--parrot-config=$parrot_config")), "\n";

system($^X, 'Configure.pl', "--parrot-config=$parrot_config");
system('make')                  and die $?;
system('make', 'install')       and die $?;
system("git rev-parse HEAD | cut -b 1,2,3,4,5,6 > $home$other/rakudo-revision") and warn $?;

eval {
	chdir glob('~/blizkost/');
	system('git', 'pull')           and warn $?;
	system($^X, 'Configure.pl', "--parrot-config=$parrot_config")
		and warn $?;
	system('make') 			and warn $?;
	system('make', 'install') 	and warn $?;
};


chdir $home;
unlink $link;
symlink $other, $link;


