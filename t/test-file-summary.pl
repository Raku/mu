#!/usr/bin/perl
use strict;
use warnings;

use File::Find;
use Data::Dumper;


my %search = (
   'spec'               => {},
   'pugs'               => {},
   'xx-uncategorized'   => {},
   'unspecced'          => {},
   'examples'           => {},
   '01-sanity'          => {},
   '02-test-pm'         => {},
);
my $re = join '|', keys %search;
$re = qr{/($re)/};
#print $re, $/;

find(\&wanted,  (shift @ARGV) || '.');
my @others;
sub wanted {
    if (m/\.svn\b/) {
        $File::Find::prune = 1;
        return
    }
    return unless m/\.t$/;
    my $n = $File::Find::name;

    my $plan = 0;
    open (my $h, '<', $_) or die "Can't open '$_': $!";
    {
        local $/ = undef;
        my $f = <$h>;
        if ($f =~ m/plan\s+(\d+)/){
            $plan = $1;
        }
    }
    close $h;

    if ( $n =~ m{$re} ) {
        $search{$1}{files}++;
        $search{$1}{plan} += $plan;
    } else {
        $search{other}{files}++;
        $search{other}{plan} += $plan;
        push @others, [$plan, $n];
    }
}

print "          Category  Files    Plan\n";
print "---------------------------------\n";
for (sort keys %search) {
    printf "%18s  %5d  %6d\n", $_, $search{$_}{files}, $search{$_}{plan};
}
print "\n";

@others = reverse sort { $a->[0] <=> $b->[0] } @others;
for (1..10) {
    printf "%4d %s\n", $others[$_]->[0], $others[$_]->[1];
}

#print Dumper \%search;
