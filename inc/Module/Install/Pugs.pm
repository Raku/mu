package Module::Install::Pugs;
use Module::Install::Base; @ISA = qw(Module::Install::Base);
use strict;
use Config;
use File::Spec;

sub WritePugs {
    my $self = shift;
    my $base = $self->{_top}{base};
    $self->set_blib("$base/blib6");
    $self->WriteAll(@_);
    $self->pugs_fix_makefile;
}

sub set_blib {
    my $self = shift;
    my $path = shift;
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
    $self->require_pugs_config;
    my $libs = PugsConfig->pugs_install_libs;
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
    $makefile =~ s/\b(runtests \@ARGV|test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q*$full_pugs*; ENV->{PERL6LIB} = q*$full_blib*; $1/;
    $makefile =~ s/("-MExtUtils::Command::MM")/"-Iinc" $1/g;
    close MAKEFILE;
    open MAKEFILE, '> Makefile' or die $!;
    print MAKEFILE $makefile;
    close MAKEFILE;
}

sub require_pugs_config {
    my $self = shift;
    my $base = $self->{_top}{base};
    eval "use lib '$base/util'; 1" or die $@;
    eval "use PugsConfig; 1" or die $@;
}

sub pugs_binary {
    my $self = shift;
    my $pugs = "pugs$Config{_exe}";
    my $base = $self->{_top}{base};
    "$base/blib/script/$pugs";
}

1;
