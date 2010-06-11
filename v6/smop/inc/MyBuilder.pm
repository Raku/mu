package MyBuilder;
use base 'Module::Build';

use warnings;
use strict;

use Config;
use Carp;

use Config::AutoConf;
use Config::AutoConf::Linker;

use ExtUtils::ParseXS;
use ExtUtils::Mkbootstrap;

use File::Spec::Functions qw.catdir catfile.;
use File::Path qw.mkpath.;
use TAP::Harness;
use v5.10;

sub ACTION_install {
    my $self = shift;
    $self->SUPER::ACTION_install;
    if ($^O =~ /linux/) {
        my $linux = Config::AutoConf->check_prog("ldconfig");
        system $linux if (-x $linux);
    }
}

sub ACTION_code {
    my $self = shift;
#
#    for my $path (catdir("blib","bindoc"), catdir("blib","bin")) {
#        mkpath $path unless -d $path;
#    }
#
#    my $version = $self->notes('btparse_version');
#    my $alloca_h = 'undef HAVE_ALLOCA_H';
#    $alloca_h = 'define HAVE_ALLOCA_H 1' if Config::AutoConf->check_header("alloca.h");
#    _interpolate("btparse/src/bt_config.h.in",
#                 "btparse/src/bt_config.h",
#                 PACKAGE  => "\"libbtparse\"",
#                 FPACKAGE => "\"libbtparse $version\"",
#                 VERSION  => "\"$version\"",
#                 ALLOCA_H => $alloca_h
#                );
#
#
#    $self->dispatch("create_manpages");
    $self->dispatch("create_objects");
    $self->dispatch("create_library");
#    $self->dispatch("create_binaries");
    $self->dispatch("create_tests");
#
#    $self->dispatch("compile_xscode");

    $self->SUPER::ACTION_code;
}

sub ACTION_compile_xscode {
#    my $self = shift;
#    my $cbuilder = $self->cbuilder;
#
#    my $archdir = catdir( $self->blib, 'arch', 'auto', 'Text', 'BibTeX');
#    mkpath( $archdir, 0, 0777 ) unless -d $archdir;
#
#    print STDERR "\n** Preparing XS code\n";
#    my $cfile = catfile("xscode","BibTeX.c");
#    my $xsfile= catfile("xscode","BibTeX.xs");
#
#    $self->add_to_cleanup($cfile); ## FIXME
#    if (!$self->up_to_date($xsfile, $cfile)) {
#        ExtUtils::ParseXS::process_file( filename   => $xsfile,
#                                         prototypes => 0,
#                                         output     => $cfile);
#    }
#
#    my $ofile = catfile("xscode","BibTeX.o");
#    $self->add_to_cleanup($ofile); ## FIXME
#    if (!$self->up_to_date($cfile, $ofile)) {
#        $cbuilder->compile( source               => $cfile,
#                            include_dirs         => [ catdir("btparse","src") ],
#                            object_file          => $ofile);
#    }
#
#    # Create .bs bootstrap file, needed by Dynaloader.
#    my $bs_file = catfile( $archdir, "BibTeX.bs" );
#    if ( !$self->up_to_date( $ofile, $bs_file ) ) {
#        ExtUtils::Mkbootstrap::Mkbootstrap($bs_file);
#        if ( !-f $bs_file ) {
#            # Create file in case Mkbootstrap didn't do anything.
#            open( my $fh, '>', $bs_file ) or confess "Can't open $bs_file: $!";
#        }
#        utime( (time) x 2, $bs_file );    # touch
#    }
#
#    my $objects = $self->rscan_dir("xscode",qr/\.o$/);
#    # .o => .(a|bundle)
#    my $lib_file = catfile( $archdir, "BibTeX.$Config{dlext}" );
#    if ( !$self->up_to_date( [ @$objects ], $lib_file ) ) {
#        $cbuilder->link(
#                        module_name => 'Text::BibTeX',
#                        extra_linker_flags => '-Lbtparse/src -lbtparse ',
#                        objects     => $objects,
#                        lib_file    => $lib_file,
#                       );
#    }
}

sub ACTION_create_manpages {
#    my $self = shift;
#
#    print STDERR "\n** Creating Manpages\n";
#
#    my $pods = $self->rscan_dir(catdir("btparse","doc"), qr/\.pod$/);
#
#    my $version = $self->notes('btparse_version');
#    for my $pod (@$pods) {
#        my $man = $pod;
#        $man =~ s!.pod!.1!;
#        $man =~ s!btparse/doc!blib/bindoc!;   ## FIXME - path
#        next if $self->up_to_date($pod, $man);
#        ## FIXME
#        `pod2man --section=1 --center="btparse" --release="btparse, version $version" $pod $man`;
#    }
#
#    my $pod = 'btool_faq.pod';
#    my $man = catfile('blib','bindoc','btool_faq.1');
#    unless ($self->up_to_date($pod, $man)) {
#        ## FIXME
#        `pod2man --section=1 --center="btparse" --release="btparse, version $version" $pod $man`;
#    }
}

sub ACTION_create_objects {
    my $self = shift;
    my $cbuilder = $self->cbuilder;
#
    print STDERR "\n** Compiling C files\n";
#    my $c_progs = $self->rscan_dir('btparse/progs', qr/\.c$/);
#    my $c_src   = $self->rscan_dir('btparse/src',   qr/\.c$/);
#    my $c_tests = $self->rscan_dir('btparse/tests', qr/\.c$/);
#    my $c_xs    = $self->rscan_dir('xscode/',       qr/\.c$/);
#
    my @c_files = @{$self->rscan_dir('s0native',       qr/\.c$/)};
    #(@$c_progs, @$c_src, @$c_tests, @$c_xs);
    mkdir('builddir');
    mkdir('builddir/s0native');
    mkdir('builddir/s0native/src');
    mkdir('builddir/s0native/t');
    for my $file (@c_files) {
        my $object = $file;
        $object =~ s/\.c/.o/;
        $object =~ s/^/builddir\//;
        next if $self->up_to_date($file, $object);
        $cbuilder->compile(object_file  => $object,
                           source       => $file,
                           include_dirs => ["base/include","s0native/include"]);
    }
}


sub ACTION_create_binaries {
#    my $self = shift;
#    my $cbuilder = $self->cbuilder;
#
#    my $EXEEXT = $Config::AutoConf::EXEEXT;
#
#    my ($LD,$CCL) = Config::AutoConf::Linker::detect_library_link_commands($cbuilder);
#    die "Can't get a suitable way to compile a C library\n" if (!$LD || !$CCL);
#
#    print STDERR "\n** Creating binaries (dumpnames$EXEEXT, biblex$EXEEXT, bibparse$EXEEXT)\n";
#
#    my @toinstall;
#    my $exe_file = catfile("btparse","progs","dumpnames$EXEEXT");
#    push @toinstall, $exe_file;
#    my $object   = catfile("btparse","progs","dumpnames.o");
#    if (!$self->up_to_date($object, $exe_file)) {
#        $CCL->($cbuilder,
#               exe_file => $exe_file,
#               objects  => [ $object ],
#               extra_linker_flags => '-Lbtparse/src -lbtparse ');
#    }
#
#    $exe_file = catfile("btparse","progs","biblex$EXEEXT");
#    push @toinstall, $exe_file;
#    $object   = catfile("btparse","progs","biblex.o");
#    if (!$self->up_to_date($object, $exe_file)) {
#        $CCL->($cbuilder,
#               exe_file => $exe_file,
#               objects  => [ $object ],
#               extra_linker_flags => '-Lbtparse/src -lbtparse ');
#    }
#
#    $exe_file = catfile("btparse","progs","bibparse$EXEEXT");
#    push @toinstall, $exe_file;
#    $object   = [map {catfile("btparse","progs","$_.o")} (qw.bibparse args getopt getopt1.)];
#    if (!$self->up_to_date($object, $exe_file)) {
#        $CCL->($cbuilder,
#               exe_file => $exe_file,
#               extra_linker_flags => '-Lbtparse/src -lbtparse ',
#               objects => $object);
#    }
#
#    for my $file (@toinstall) {
#        $self->copy_if_modified( from    => $file,
#                                 to_dir  => "blib/bin",
#                                 flatten => 1);
#    }

}

sub ACTION_create_tests {
    my $self = shift;
    my $cbuilder = $self->cbuilder;

    my $EXEEXT = $Config::AutoConf::EXEEXT;

    my ($LD,$CCL) = Config::AutoConf::Linker::detect_library_link_commands($cbuilder);
    die "Can't get a suitable way to compile a C library\n" if (!$LD || !$CCL);

    print STDERR "\n** Creating test binaries\n";

    mkdir('builddir/t');
    for my $module (qw(s0native)) {
        for my $test (@{$self->rscan_dir("builddir/$module/t" ,       qr/\.o$/)}) {
            my $exe_file = $test;
            $exe_file =~ s/\.o$/$EXEEXT/;
            $exe_file =~ s{builddir/\Q$module}{builddir};
            say $exe_file;
            if (!$self->up_to_date([$test], $exe_file)) {
                $CCL->($cbuilder,
                    exe_file => $exe_file,
                    extra_linker_flags => '-Lbuild/lib -lsmop-s0native',
                    objects => [$test]);
            }
        }
    }
#
#    my $objects  = [ catfile("build","s0native","t","const_indentifier.o") ];
}

sub ACTION_create_library {
    my $self = shift;
    my $cbuilder = $self->cbuilder;
    my ($LD,$CCL) = Config::AutoConf::Linker::detect_library_link_commands($cbuilder);
    die "Can't get a suitable way to compile a C library\n" if (!$LD || !$CCL);
    my $LIBEXT = $Config::AutoConf::LIBEXT;

#    print STDERR "\n** Creating libbtparse$LIBEXT\n";

    mkdir('builddir/lib');
    for my $module (qw(s0native)) {
        my @objects = @{$self->rscan_dir("builddir/$module/src",qr/\.o$/)};
        # FIXME libpath
        my $libpath = $self->notes('lib_path');
        my $libfile = "builddir/lib/libsmop-$module$LIBEXT";
        if (!$self->up_to_date(\@objects, $libfile)) {
            $LD->($cbuilder,
                module_name => 'smop-' . $module,
                ($^O =~ /darwin/)?(extra_linker_flags => "-install_name $libpath"):(),
                objects => \@objects,
                lib_file => $libfile);
        }
    }

#
#    $libpath = catfile($libpath, "libbtparse$LIBEXT");
#
#
#    my $libdir = catdir($self->blib, 'usrlib');
#    mkpath( $libdir, 0, 0777 ) unless -d $libdir;
#
#    $self->copy_if_modified( from   => $libfile,
#                             to_dir => $libdir,
#                             flatten => 1 );
}

sub ACTION_test {
    my $self = shift;

    my $path = catdir('builddir','lib');
    if ($^O =~ /darwin/i) {
        $ENV{DYLD_LIBRARY_PATH} = $path;
    }
    if ($^O =~ /(freebsd|solaris|linux)/i) {
        $ENV{LD_LIBRARY_PATH} = $path;
    }
    my $harness = TAP::Harness->new({ exec=>[] });
    $harness->runtests(glob('builddir/t/*'));
#    $harness->runtests( @{ $self->rscan_dir("builddir//src",qr/\.o$/)}
#    system('builddir/s0native/t/const_identifier');

#    $self->SUPER::ACTION_test
}


sub _interpolate {
    my ($from, $to, %config) = @_;
	
    print "Creating new '$to' from '$from'.\n";
    open FROM, $from or die "Cannot open file '$from' for reading.\n";
    open TO, ">", $to or die "Cannot open file '$to' for writing.\n";
    while (<FROM>) {
        s/\[%\s*(\S+)\s*%\]/$config{$1}/ge;		
        print TO;
    }
    close TO;
    close FROM;
}
1;
