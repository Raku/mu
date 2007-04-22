#!/usr/bin/perl
#
# diff of two smoke.yml files
# give them as arguments on the command line, otherwise they default to
# "smoke.yml" and "smoke.last.yml";
#
# Written by Moritz Lenz, moritz at faui2k3 dot org
use warnings;
use strict;
use YAML::Syck;
use Data::Dumper;

my $fn1 = shift @ARGV || "smoke.last.yml";
my $fn2 = shift @ARGV || "smoke.yml";

my $d1 = LoadFile($fn1);
my $d2 = LoadFile($fn2);

my @f1 = @{$d1->{meat}{test_files}};
my @f2 = @{$d2->{meat}{test_files}};
my %res;
my (@passes, @fails);

my $diff = 0;
for (my $i = 0; $i < @f1; $i++){
    my $f1 = $f1[$i];
    my $f2 = $f2[$i + $diff];
    if ($f1->{file} ne $f2->{file}){
        for (-2 .. 2){
            if ($f1[$i]->{file} eq $f2[$i + $diff + $_]->{file}){
                $diff = $diff + $_;
                last;
            }
        }
    }
    if ($f1->{file} eq $f2->{file}){
        if (scalar @{$f1->{events}} != scalar @{$f2->{events}}){
            print "Number of tests in $f1->{file} changed\n";
        } else {
            my $count = scalar @{$f1->{events}};
            foreach my $k (0 .. $count - 1){
                my ($ok1, $ok2) = ($f1->{events}[$k]{actual_ok}, $f2->{events}[$k]{actual_ok});
                if ($ok1 ne $ok2){
                    print $f1->{events}[$k]{pos}, " changed from $ok1 to $ok2\n";
                    if ($ok1){
                        push @passes, $f1->{events}[$k]{pos};
                    } else {
                        push @fails, $f1->{events}[$k]{pos};
                    }
                }
            }
        }
    } else {
        print "New test file: ", $f1->{file}, "\n";
    }
}
