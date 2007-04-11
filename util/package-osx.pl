#!perl -l012

=pod
=head1 NAME

package-osx.pl - Build distribution of pugs w/embedded perl5 and parrot

=head1 SYNOPSIS

perl package-osx.pl

Options:

    --prefix        Installation parent directory
    --readline     Readline build directory
    --perl          Perl build directory
    --parrot        Parrot build directory
    --ghc           GHC build directory
    --gmp           Directory of GMP Framework
    --help          This message

=head1 OPTIONS

=over 8

=item B<-prefix>

Pugs, et c. are installed in a subdirectory of B<-prefix>, which is the
revision number of Pugs. For instance, if you pass in /usr/local/pugs for
B<-prefix>, and the current Pugs revision is 12321, then Pugs and friends
will be installed under /usr/local/pugs/r12321.

=item B<-readline>

The readline build directory if you want to use that version of readline for
parrot, ghc, and pugs rather than the system's version (OS X has a broken
one).

=item B<-perl>

The perl build directory if you want to bundle a perl installation with pugs.
It will be installed under B<-prefix>, and then used to build parrot (if
specified) and pugs.

=item B<-parrot>

The parrot build directory if you want to embed parrot (e.g., /src/parrot).

=item B<-ghc>

The ghc build directory if you want to bundle ghc with pugs and use it to
build pugs. B<TODO:> Haskell can't be embedded in pugs because I haven't built
hs-plugins.

=item B<-gmp>

The path to the GMP framework that you want to bundle with pugs. If not used,
the pugs and ghc will link to the system's GMP.framework.

=back

=head1 DESCRIPTION

You must have GHC and Perl versions needed for Pugs in order to be built. Pugs
needs both of these in order to be built. The options that are passed in
(e.g., B<-ghc>) will be used in the build process, but otherwise you need GHC
and Perl in your PATH.  (GHC needs GHC in order to be built, so you must have
a version lying around somewhere :-)

You must also have wget and patch in your PATH, or just change the fetching,
patching, and installing of readline yourself.

=cut
use FindBin qw/$Bin/;
use File::Spec;
use Getopt::Long;
use Pod::Usage;

my @build_dirs = qw/readline perl parrot ghc gmp/;
my @relink_dirs = qw/readline gmp/;
our %options;
GetOptions \%options, qw/help prefix=s/, map "$_=s", @build_dirs
    or pod2usage(2);
pod2usage(-verbose => 2) if $options{'help'};
s/^~/$ENV{HOME}/ for values %options;

chdir $Bin && qx/$^X version_h.pl/;
my $version_h_file = File::Spec->catfile( File::Spec->updir,"src", "Pugs",
    "pugs_version.h");
open my $version_h, '<', $version_h_file or die "Cannot open $version_h_file!";
my ($version) = join '', 'r', grep $_, map { /\d+/g } <$version_h>;
my $install_dir = File::Spec->catfile( $options{'prefix'}, $version );


my $perl_exe = $^X;

my $log_file = File::Spec->catfile( $Bin, 'package-osx.pl.log' );

open my $log, '>', $log_file or die "Cannot open $log_file!";

my @build = grep $options{$_}, @build_dirs;
chdir $options{$_} or die "Can't chdir to $options{$_}!\n" for @build;
for my $pkg ( @build ) {
    eval "install_$pkg()";
    die $@ if $@;
}
install_pugs();

my @relink = grep $options{$_}, @relink_dirs;
for my $pkg ( @relink ) {
    eval "relink_$pkg()";
    die $@ if $@;
}

create_pkg();

create_dmg();

close $log;

sub install_readline {
    $_ = "Installing Readline from $options{'readline'}";
    chdir $options{'readline'};
    print, print $log $_, '-' x length;
    print, print $log $_, qx/$_/ for "sh ./configure --prefix=$install_dir 2>&1",
        'make 2>&1', 'make install 2>&1';
}

sub install_perl {
    $_ = "Installing Perl from $options{'perl'}";
    chdir $options{perl};
    print, print $log $_, '-' x length;
    print, print $log $_, qx/$_/ for 'rm -f config.sh Policy.sh',
        "sh Configure -de -Dprefix=$install_dir 2>&1", 'make 2>&1',
        'make install 2>&1';
    $perl_exe = File::Spec->catfile( $install_dir, 'bin', 'perl' );
    $ENV{PUGS_EMBED} .= " perl5";
}

sub install_parrot {
    $_ = "Installing Parrot from $options{'parrot'}";
    chdir $options{parrot};
    print, print $log $_, '-' x length;
    print, print $log $_, qx/$_/ for "$perl_exe Configure.pl --prefix=$install_dir 2>&1",
        'make 2>&1', 'make reallyinstall 2>&1';
    $ENV{PARROT_PATH} = $options{parrot};
    $ENV{PUGS_EMBED} .= " parrot";
}

sub install_ghc {
    $_ = "Installing GHC from $options{'ghc'}";
    chdir $options{ghc};
    print, print $log $_, '-' x length;
    $ENV{LDFLAGS} = "-L$install_dir/lib $ENV{LDFLAGS}";
    $ENV{CPPFLAGS} = "-I$install_dir/include $ENV{CPPFLAGS}";
    print, print $log $_, qx/$_/ for "sh ./configure --prefix=$install_dir 2>&1",
        'make 2>&1', 'make install 2>&1';
    $ENV{GHC} = File::Spec->catfile( $install_dir, 'bin', 'ghc' );
    # TODO hs-plugins install
    # $ENV{PUGS_EMBED} .= " $_";
}

# install GMP to pugs dir
sub install_gmp {
    print, print $log $_, qx/$_/ for
        "cp -r $options{gmp} $install_dir/lib/GMP.framework 2>&1",
        "touch $install_dir/lib/GMP.framework/build-framework.sh.Emm 2>&1";
}

# install pugs
sub install_pugs {
    chdir $Bin && chdir File::Spec->updir;
    print, print $log $_, qx/$_/
        for "$perl_exe Makefile.PL 2>&1", "make install 2>&1";
}

# change libreadline link if needed
sub relink_readline {
    chdir $install_dir;
    for my $util ( grep !/(readline|history)/ && -f, <lib/*>,  <bin/*> ) {
        $_ = `otool -L $install_dir/$util`;
        next if /not an object file/;

        my ($rl_lib) = grep $_, map { /\S+libreadline\S+/g } $_;
        next unless $rl_lib;

        my $mode_orig = (stat $util)[2];
        chmod 0777, $util or die "Cannot chmod $_ to 0777!\n";
        my $change = "-change $rl_lib $install_dir/lib/libreadline.dylib";
        print $log $_, qx/$_/
            for "install_name_tool $change $install_dir/$util";
        chmod $mode_orig, $util;
    }
}

# change GMP.framework link if needed
sub relink_gmp {
    chdir $install_dir;
    for my $util ( grep -f, <lib/*>,  <bin/*> ) {
        $_ = `otool -L $install_dir/$util`;
        next if /not an object file/;

        my ($gmp_lib) = grep $_, map { /\S*GMP\.framework\S*/g } $_;
        next unless $gmp_lib;

        my $mode_orig = (stat $util)[2];
        chmod 0777, $util or die "Cannot chmod $_ to 0777!\n";

        my $change = "-change $gmp_lib $install_dir/lib/$gmp_lib";
        print $log $_, qx/$_/
                for "install_name_tool $change $install_dir/$util";
        chmod $mode_orig, $util;
    }
}

sub create_pkg {
    chdir $Bin;
    print $log $_, qx/$_/ for "tar -P --preserve -zcf pugs-$version.tar.gz $install_dir";
    return;

    # TODO need to fill in create_pkg_resources and create_pkg_info
    # create root directory
    for ( "package-osx", grep $_, File::Spec->splitdir( $install_dir ) ) {
        mkdir($_, 0777) && chdir $_ or die "Cannot chdir to $_\n";
    }
    `ditto $install_dir $Bin/package-osx/$install_dir`;

    chdir $Bin;

    create_pkg_resources();

    create_pkg_info();

    my $pmaker = File::Spec->catfile( qw/Developer Applications Utilities
        PackageMaker.app Contents MacOS PackageMaker /);
    print $log $_, qx/$_/
        for join " ", $pmaker, "-build", "-p pugs-$version.pkg",
            "-f $Bin/osx/root", "-r $Bin/osx/Resources",
            "-i $Bin/osx/info.plist";
}

sub create_pkg_resources {
    # TODO
}

sub create_pkg_info {
    # TODO
}

sub create_dmg {
    # TODO

}
