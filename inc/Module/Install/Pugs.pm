package Module::Install::Pugs;
use Module::Install::Base; @ISA = qw(Module::Install::Base);
use strict;
use Config;
use File::Spec;
use File::Basename;

sub WritePugs {
    my $self = shift;
    $self->WriteAll(@_);
    $self->pugs_fix_makefile;
}

sub base_path {
    my $self = shift;
    $self->{_top}{base};
}

sub set_blib {
    my $self = shift;
    my $perl_version = shift 
      or die "Must pass Perl version (5 or 6)";
    my $blib = ($perl_version == 5)
    ? 'blib'
    : $perl_version == 6
      ? 'blib6'
      : die "Perl version '$perl_version' is bad. Must be 5 or 6.";
    my $base = $self->{_top}{base};
    my $path = File::Spec->catdir($base, $blib);
    $self->makemaker_args->{'INST_LIB'} = 
      File::Spec->catfile($path, "lib");
    $self->makemaker_args->{'INST_ARCHLIB'} = 
      File::Spec->catfile($path, "arch");
    $self->makemaker_args->{'INST_SCRIPT'} = 
      File::Spec->catfile($path, "script");
    $self->makemaker_args->{'INST_BIN'} = 
      File::Spec->catfile($path, "bin");
    $self->makemaker_args->{'INST_MAN1DIR'} = 
      File::Spec->catfile($path, "man1");
    $self->makemaker_args->{'INST_MAN3DIR'} = 
      File::Spec->catfile($path, "man3");
}

sub setup_perl6_install {
    my $self = shift;
    my $libs = $self->get_pugs_config;
    $self->makemaker_args(
        INSTALLARCHLIB => $libs->{archlib},
        INSTALLPRIVLIB => $libs->{privlib},
    );
}

sub pugs_fix_makefile {
    my $self = shift;
    my $base = $self->{_top}{base};
    my $full_pugs = $self->pugs_binary;
    my $full_blib = File::Spec->catfile($base, 'blib6', 'lib');
    open MAKEFILE, '< Makefile' or die $!;
    my $makefile = do { local $/; <MAKEFILE> };
    $full_pugs =~ s{\\}{\\\\}g; 
    $full_pugs =~ s{'}{\\'}g;
    $full_blib =~ s{\\}{\\\\}g; 
    $full_blib =~ s{'}{\\'}g;
    $makefile =~ s/\b(runtests \@ARGV|test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q*$full_pugs*; \@ARGV = grep !\/[A-Z]\/, map glob, \@ARGV; ENV->{PERL6LIB} = q*$full_blib*; $1/;
    $makefile =~ s/("-MExtUtils::Command::MM")/"-Iinc" $1/g;
    $makefile =~ s/\$\(UNINST\)/0/g;
    close MAKEFILE;
    open MAKEFILE, '> Makefile' or die $!;
    print MAKEFILE $makefile;
    close MAKEFILE;
}

sub get_pugs_config {
    my $self = shift;
    my $base = $self->{_top}{base};
    eval "use lib '$base/util'; 1" or die $@;
    eval "use PugsConfig; 1" or die $@;
    PugsConfig->get_config;
}

sub pugs_binary {
    my $self = shift;
    my $pugs = "pugs$Config{_exe}";
    my $base = $self->{_top}{base};
    "$base/blib/script/$pugs";
}

sub deny_cygwin {
    if ($^O eq 'cygwin') {
	die << "."
** Cygwin currently unsupported on Win32; please use ActivePerl or MinGW Perl
   instead, with the .msi installer of GHC.
.
    }
}

sub assert_ghc {
    my $self = shift;
    my $ghc = $self->can_run($ENV{GHC} || 'ghc');
    my $ghcver = `$ghc --version`;
    ($ghcver =~ /Glasgow.*\bversion\s*(\S+)/s) or die << '.';
*** Cannot find a runnable 'ghc' from path.
*** Please install GHC from http://haskell.org/ghc/.
.

    return ($ghc, $1);
}

sub fixpaths {
    my $self = shift;
    my $text = shift;
    my $sep = File::Spec->catdir('');
    $text =~ s{\b/}{$sep}g;
    return $text;
}

1;
