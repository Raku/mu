package Perl6::MakeMaker;
use strict;
use warnings;
our $VERSION = '0.01';
use base 'Exporter';
our @EXPORT = qw(WriteMakefile);

use ExtUtils::MakeMaker();
use File::Spec;
use Config;

sub WriteMakefile {
    my $libs = get_perl6_libs();
    my @mm_args = @_;
    @mm_args = handle_inline(@mm_args);
    ExtUtils::MakeMaker::WriteMakefile(
        INSTALLSITELIB  => $libs->{sitelib},
        INSTALLSITEARCH => $libs->{sitearch},
        @mm_args,
    );
    fix_makefile();
}

sub get_perl6_libs {
    my $pugs_path = shift || 'pugs';
    my $output = `$pugs_path -V`;
    my ($archlib) = ($output =~ /^\s*archlib:\s+(\S+)/m);
    my ($privlib) = ($output =~ /^\s*privlib:\s+(\S+)/m);
    my ($sitearch) = ($output =~ /^\s*sitearch:\s+(\S+)/m);
    my ($sitelib) = ($output =~ /^\s*sitelib:\s+(\S+)/m);
    return {
        archlib => $archlib,
        privlib => $privlib,
        sitearch => $sitearch,
        sitelib => $sitelib,
    };
}

my $postamble = '';
sub fix_makefile {
    my $full_pugs = File::Spec->catfile($Config{installbin}, 'pugs');
    my $full_blib = File::Spec->rel2abs(File::Spec->catfile('blib', 'lib'));
    open MAKEFILE, '< Makefile' or die $!;
    my $makefile = do { local $/; <MAKEFILE> };
    $full_pugs =~ s{\\}{\\\\}g; 
    $full_pugs =~ s{'}{\\'}g;
    $full_blib =~ s{\\}{\\\\}g; 
    $full_blib =~ s{'}{\\'}g;
    $makefile =~ s/\b(runtests \@ARGV|test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q{$full_pugs}; ENV->{PERL6LIB} = q{$full_blib}; $1/;
    $makefile =~ s/("-MExtUtils::Command::MM")/"-Iinc" $1/g;
    close MAKEFILE;
    open MAKEFILE, '> Makefile' or die $!;
    print MAKEFILE $makefile;
    print MAKEFILE $postamble;
    close MAKEFILE;
}

sub handle_inline {
    my %args = @_;
    if ($args{inline}) {
        external($args{inline});
        delete $args{inline};
    }
    return %args;
}

sub external {
    my $module_path = shift;
    open MODULE, $module_path
      or die "Can't open '$module_path' for input\n";
    my $source = do { local $/; <MODULE> };
    return unless $source =~
    /^\s*module\s+(.*?);.*\sinline\s/ms;
    my $module_name = $1;
    $module_name =~ s/-/__/;
    $module_name =~ s/[\-\.]/_/g;

    my ($ghc, $ghc_version, $ghc_flags) = assert_ghc();

    $postamble = <<_;
pure_all :: $module_name.o $module_name.hi
	\$(CP) $module_name.o \$(INST_ARCHLIB)
	\$(CP) $module_name.hi \$(INST_ARCHLIB)

$module_name.o :: $module_name.hs
	$ghc --make -isrc -Isrc $ghc_flags \$(GHC_FLAGS) $module_name.hs

$module_name.hs :: $module_path
	pugs --external $module_name $module_path > $module_name.hs
_
}

sub assert_ghc {
    my $ghc = can_run($ENV{GHC} || 'ghc') or die;
    my $ghcver = `$ghc --version`;
    ($ghcver =~ /Glasgow.*\bversion\s*(\S+)/s) or die << '.';
*** Cannot find a runnable 'ghc' from path.
*** Please install GHC from http://haskell.org/ghc/.
.
    my $ghc_version = $1;

    my $config = get_perl6_libs();
    my $pugs_core = File::Spec->catdir($config->{archlib}, 'CORE', 'pugs');
    my $ghc_flags = "-H200m -L. -Lsrc -Lsrc/pcre -I. -Isrc -Isrc/pcre -i.  -isrc -isrc/pcre -static -fno-warn-missing-signatures -fno-warn-name-shadowing -I$pugs_core -L$pugs_core -i$pugs_core";
    return ($ghc, $ghc_version, $ghc_flags);
}

sub can_run {
    my $cmd = shift;

    my $_cmd = $cmd;
    return $_cmd if (-x $_cmd or $_cmd = MM->maybe_command($_cmd));

    for my $dir ((split /$Config::Config{path_sep}/, $ENV{PATH}), '.') {
        next unless $dir;
        my $abs = File::Spec->catfile($dir, $cmd);
        return $abs if (-x $abs or $abs = MM->maybe_command($abs));
    }

    return;
}

1;
