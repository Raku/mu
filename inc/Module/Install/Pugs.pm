package Module::Install::Pugs;
use Module::Install::Base; @ISA = qw(Module::Install::Base);
use strict;
use Config;
use File::Spec;
use File::Basename;

sub WritePugs {
    my $self = shift;
    
    my $install_version = shift;
    die "Install version must be 5 or 6 for WritePugs"
      unless $install_version =~ /^[56]$/;

    $self->setup_perl6_install 
      if $install_version eq '6';

    $self->set_blib($install_version);

    $self->set_makefile_macros
      if $install_version eq '6';

    $self->WriteAll(@_);

    $self->pugs_fix_makefile;
}

sub set_makefile_macros {
    my $self = shift;

    package MM;
    *init_INST = sub {
        my $hash = $self->{MM};
        my $mm = shift;
        $mm->SUPER::init_INST(@_);
        for (keys %$hash) {
            $mm->{$_} = $hash->{$_};
        }
        return 1;
    }
}


sub base_path {
    my $self = shift;
    $self->{_top}{base};
}

sub is_extension_build {
    my $self = shift;
    not -e $self->base_path . "/lib/Perl6/Pugs.pm";
}

sub set_blib {
    my $self = shift;
    my $perl_version = shift 
      or die "Must pass Perl version (5 or 6)";
    my $base = $self->{_top}{base};
    my $blib = ($perl_version == 5 || $self->is_extension_build)
    ? 'blib'
    : $perl_version == 6
      ? 'blib6'
      : die "Perl version '$perl_version' is bad. Must be 5 or 6.";
    my $path = File::Spec->catdir($base, $blib);

    $self->makemaker_args->{INST_LIB} = 
      File::Spec->catfile($path, "lib");
    $self->makemaker_args->{INST_ARCHLIB} = 
      File::Spec->catfile($path, "arch");
    $self->makemaker_args->{INST_SCRIPT} = 
      File::Spec->catfile($path, "script");
    $self->makemaker_args->{INST_BIN} = 
      File::Spec->catfile($path, "bin");
    $self->makemaker_args->{INST_MAN1DIR} = 
      File::Spec->catfile($path, "man1");
    $self->makemaker_args->{INST_MAN3DIR} = 
      File::Spec->catfile($path, "man3");
    $self->{MM}{INST_AUTODIR} = '$(INST_LIB)/$(BASEEXT)';
    $self->{MM}{INST_ARCHAUTODIR} = '$(INST_ARCHLIB)/$(FULLEXT)';
}

sub setup_perl6_install {
    my $self = shift;
    my $libs = $self->get_pugs_config;
    $self->makemaker_args(
        INSTALLARCHLIB  => $libs->{archlib},
        INSTALLPRIVLIB  => $libs->{privlib},
        INSTALLSITEARCH => $libs->{sitearch},
        INSTALLSITELIB  => $libs->{sitelib},
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
    $makefile =~ s/\b(runtests \@ARGV|test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q{$full_pugs}; \@ARGV = grep !\/[A-Z]\/, map glob, \@ARGV; ENV->{PERL6LIB} = q{$full_blib}; $1/;
    $makefile =~ s!("-MExtUtils::Command::MM")!"-I../../inc" "-I../inc" "-Iinc" $1!g;
    $makefile =~ s/\$\(UNINST\)/0/g;
    close MAKEFILE;
    open MAKEFILE, '> Makefile' or die $!;
    print MAKEFILE $makefile;
    close MAKEFILE;
}

sub get_pugs_config {
    my $self = shift;
    my $base = $self->is_extension_build
    ? '../..'
    : $self->{_top}{base};
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

    my $ghc_version = $1;
    my $ghc_flags = "-H200m -L. -Lsrc -Lsrc/pcre -I. -Isrc -Isrc/pcre ";
    $ghc_flags .= " -i. -isrc -isrc/pcre -static "; 
    $ghc_flags .= " -Wall "
      unless $self->is_extension_build;
    $ghc_flags .= " -fno-warn-missing-signatures -fno-warn-name-shadowing ";
    $ghc_flags .= " -I../../src -i../../src "
      if $self->is_extension_build;
    $ghc_flags .= " -I$Config{archlib}/CORE -L$Config{archlib}/CORE -i$Config{archlib}/CORE -lperl" 
      if $ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /perl5/i;
    $ghc_flags .= " -fno-warn-deprecations -fno-warn-orphans"
      if $ghc_version ge '6.4';
    return ($ghc, $ghc_version, $ghc_flags);
}

sub fixpaths {
    my $self = shift;
    my $text = shift;
    my $sep = File::Spec->catdir('');
    $text =~ s{\b/}{$sep}g;
    return $text;
}

sub external {
    my $self = shift;
    my $module_path = shift;
    open MODULE, $module_path
      or die "Can't open '$module_path' for input\n";
    my $source = do { local $/; <MODULE> };
    return unless $source =~
    /^\s*module\s+(.*?);.*\sinline\s/ms;
    my $module_name = $1;
    $module_name =~ s/-/__/;
    $module_name =~ s/[\-\.]/_/g;
    $self->{MM}{FULLEXT} = $module_name;

    my ($ghc, $ghc_version, $ghc_flags) = $self->assert_ghc;

    $self->postamble(<<_);
pure_all :: $module_name.o
	cp $module_name.o \$(INST_ARCHAUTODIR)

$module_name.o :: $module_name.hs
	$ghc --make -isrc -Isrc $ghc_flags \$(GHC_FLAGS) $module_name.hs

$module_name.hs :: $module_path
	pugs --external $module_name $module_path > $module_name.hs
_
}

1;
