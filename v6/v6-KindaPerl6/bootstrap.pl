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

my %options;
getopt('a', \%options);

my @sources = map { glob $_ } qw(
        lib/KindaPerl6/*.pm 
        lib/KindaPerl6/*/*.pm
        lib/KindaPerl6/Runtime/Perl6/*.pm
        );

sub compile {
    my ($src, $obj) = @_;
    if (exists $options{a} || (!stat $obj || (stat $src)[9] > (stat $obj)[9])){
        print "Compiling $src\n";
        system "perl -Ilib kp6-perl5.pl $src > $obj";
    }
}

for (@sources){
    compile($_, "bootstrap/$_");
}

for (glob 'lib/KindaPerl6/Runtime/Perl5/*.pm'){
    copy $_, "boostrap/$_";
}
    


# vim: sw=4 ts=4 expandtab
