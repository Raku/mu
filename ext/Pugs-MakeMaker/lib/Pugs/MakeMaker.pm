package Pugs::MakeMaker;
use strict;
use warnings;
our $VERSION = '0.01';
use base 'Exporter';
our @EXPORT = qw(WriteMakefile);

use ExtUtils::MakeMaker();
use File::Spec;

sub WriteMakefile {
    my $libs = get_perl6_libs();
    ExtUtils::MakeMaker::WriteMakefile(
        INSTALLSITELIB => $libs->{sitelib},
        INST_ARCHLIB => File::Spec->catfile('blib6', 'arch'),
        INST_SCRIPT => File::Spec->catfile('blib6', 'script'),
        INST_BIN => File::Spec->catfile('blib6', 'bin'),
        INST_LIB => File::Spec->catfile('blib6', 'lib'),
        INST_MAN1DIR => File::Spec->catfile('blib6', 'man1'),
        INST_MAN3DIR => File::Spec->catfile('blib6', 'man3'),
        @_,
    );
    fix_makefile();
}

sub get_perl6_libs {
    my $pugs_path = shift || 'pugs';
    my $output = `$pugs_path -V`;
    my ($archlib) = ($output =~ /^archlib:\s+(\S+)/m);
    my ($privlib) = ($output =~ /^privlib:\s+(\S+)/m);
    my ($sitearch) = ($output =~ /^sitearch:\s+(\S+)/m);
    my ($sitelib) = ($output =~ /^sitelib:\s+(\S+)/m);
    return {
        archlib => $archlib,
        privlib => $privlib,
        sitearch => $sitearch,
        sitelib => $sitelib,
    };
}

sub fix_makefile {
    my $full_pugs = 'pugs';
    my $full_t = "t";
    open MAKEFILE, '< Makefile' or die $!;
    my $makefile = do { local $/; <MAKEFILE> };
    $full_pugs =~ s{\\}{\\\\}g; 
    $full_pugs =~ s{'}{\\'}g;
    $full_t =~ s{\\}{\\\\}g; 
    $full_t =~ s{'}{\\'}g;
    $makefile =~ s/\b(runtests \@ARGV|test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q*$full_pugs*; ENV->{PERL6LIB} = q*$full_t*; $1/;
    $makefile =~ s/("-MExtUtils::Command::MM")/"-Iinc" $1/g;
    close MAKEFILE;
    open MAKEFILE, '> Makefile' or die $!;
    print MAKEFILE $makefile;
    close MAKEFILE;
}

1;
