#!/usr/bin/perl

=head1 NAME

bootstrap.pl

=head1 SYNOPSIS

    ./bootstrap.pl -a    # recompile everything 
    ./bootstrap.pl       # recompile changed files

=head1 DESCRIPTION

C<bootstrap.pl> compiles kp6 with itself into C<bootstrap/lib/>.

=head1 AUTHOR

Moritz Lenz and the Perl 6 contributors

=cut

use File::Copy;
use Getopt::Std;
use Data::Dumper;
use POSIX ":sys_wait_h";

my %options;
getopt('an:', \%options);

my @sources = map { glob $_ } qw(
        lib-kp6-kp6/KindaPerl6/*.pm 
        lib-kp6-kp6/KindaPerl6/*/*.pm
        lib-kp6-kp6/KindaPerl6/Runtime/Perl6/*.pm
        );

sub compile {
    my ($src, $obj) = @_;
    if (exists $options{a} || (!stat $obj || (stat $src)[9] > (stat $obj)[9])){
        print "Compiling (still not bootstrapped) $src\n";
        system "perl kp6-mp6-perl5.pl $src > $obj";
    }
}

$options{n} ||= 1;
my $number_per_process = $#sources / $options{n};
my $proc_counter = 0;
for (1..$options{n}) {
    if (my $pid = fork()) {
    } else {
        $proc_counter = $_ - 1;
        for (0..$number_per_process){
            my $file = $sources[$number_per_process * $proc_counter + $_];
            $file =~ s/^lib-kp6-kp6\///;
            compile("lib-kp6-kp6/$file", "lib-kp6-kp6-p5/$file");
        }
        exit;
    }
}
while (wait != -1) {
    print 'waiting...'.$/;
} ;
for (glob 'lib-kp6-kp6/KindaPerl6/Runtime/Perl5/*.pm'){
    copy $_, "lib-kp6-kp6-p5/$_";
}


# vim: sw=4 ts=4 expandtab
