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

use File::Spec::Functions qw(catdir catfile);
use File::Path qw(make_path);
use TAP::Harness;

use ExtUtils::Embed qw(ldopts ccopts);

use SMOP::RI::Writer;

my $ldopts = ldopts . '-lrt';

use v5.10;

my @MODULES = qw(s0native dump nagc util capture interpreter mold yeast native lost s1p p6opaque s1p-oo mold-message profile p5);

my $BUILDDIR = 'build';


sub new {
    my ($self,@args) = @_;
    for (@MODULES) {
        mkdir(catdir($_,'include'));
    }
    $self->SUPER::new(@args,'share_dir'=>[catdir('build','lib'),map {catdir($_,'include')} ('base','util',@MODULES)]);
}

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

#    $self->dispatch("create_manpages");
    $self->dispatch("create_objects");
    $self->dispatch("create_library");
#    $self->dispatch("create_binaries");
    $self->dispatch("create_tests");

    $self->dispatch("compile_xscode");

    $self->SUPER::ACTION_code;
}



sub ACTION_create_objects {
    my $self = shift;
    my $cbuilder = $self->cbuilder;
#
    print STDERR "\n** Compiling C files\n";

    my %extra_flags = (p5=>ccopts);

    my @INCLUDE = (catdir("base","include"),catdir("util","include"));
    for my $module (@MODULES) {

        make_path(catdir($BUILDDIR,$module,"src"),catdir($BUILDDIR,$module,"t"));

        # copy headers so that they can be used when compiling generated files 
        for my $header (@{$self->rscan_dir(catdir($module,"src"),qr/\.h$/)}) {
            my $dest = $header;
            $dest =~ s/^/$BUILDDIR\//;
            $self->copy_if_modified( from => $header, to => $dest);

        }

        push(@INCLUDE,"$module/include");

        my @c_files = @{$self->rscan_dir(catdir($module,"src"),qr/\.c$/)};
        for my $file (@c_files) {
            my $object = $file;
            $object =~ s/\.c/.o/;
            $object =~ s/^/$BUILDDIR\//;
            next if $self->up_to_date($file, $object);
            my $extra_flags = $extra_flags{$module} ? ' '.$extra_flags{$module} : '';
            $cbuilder->compile(object_file  => $object,
                source       => $file,
                extra_compiler_flags => $extra_flags,
                include_dirs => [@INCLUDE])
        }

        my @ri_files = @{$self->rscan_dir(catdir($module,"src"),qr/\.ri$/)};
        for my $file (@ri_files) {
            say "RI file $file";

            my $object = $file;
            $object =~ s/\.ri/.o/;
            $object =~ s/^/$BUILDDIR\//;

            my $c_file = $file;
            $c_file =~ s/\.ri/.c/;
            $c_file =~ s/^/$BUILDDIR\//;

            if (!$self->up_to_date($file, $c_file)) {
                SMOP::RI::Writer::process_ri($file,$c_file);
            }
            if (!$self->up_to_date($c_file, $object)) {
                $cbuilder->compile(object_file  => $object,
                    source       => $c_file,
                    extra_compiler_flags => "-g",
                    include_dirs => [@INCLUDE]);
            }
        }
    }
    for my $module (@MODULES) {
        my @c_files = @{$self->rscan_dir(catdir($module,"t"),       qr/\.c$/)};
        for my $file (@c_files) {
            my $object = $file;
            $object =~ s/\.c/.o/;
            $object =~ s/^/$BUILDDIR\//;
            next if $self->up_to_date($file, $object);
            $cbuilder->compile(object_file  => $object,
                source       => $file,
                extra_compiler_flags => "-g",
                include_dirs => [@INCLUDE]);
        }
    }
}



sub smop_lib_flags {
    my @LIBS;
    push(@LIBS,'-L'.catdir($BUILDDIR,'lib'));
    for my $module (@MODULES) {
        push(@LIBS,'-lsmop-' . $module);
    }
    push(@LIBS,split(' ',ldopts));
    return @LIBS;
}
sub smop_include_dirs {
    my @INCLUDE = (catdir("base","include"),catdir("util","include"));
    for my $module (@MODULES) {
        push(@INCLUDE,"$module/include");
    }
    @INCLUDE;
}
sub smop_include_flags {
    return map {"-I$_"} smop_include_dirs();
}
sub ACTION_create_tests {
    my $self = shift;
    my $cbuilder = $self->cbuilder;

    my $EXEEXT = $Config::AutoConf::EXEEXT;

    my ($LD,$CCL) = Config::AutoConf::Linker::detect_library_link_commands($cbuilder);
    die "Can't get a suitable way to compile a C library\n" if (!$LD || !$CCL);

    print STDERR "\n** Creating test binaries\n";

    mkdir(catdir($BUILDDIR,'t'));

    my $LIBS = join(' ',smop_lib_flags);

    for my $module (@MODULES) {
        for my $test (@{$self->rscan_dir("$BUILDDIR/$module/t" ,       qr/\.o$/)}) {
            my $exe_file = $test;
            $exe_file =~ s/\.o$/$EXEEXT/;
            $exe_file =~ s{$BUILDDIR/\Q$module}{$BUILDDIR};
            say $exe_file;
            if (!$self->up_to_date([$test], $exe_file)) {
                $CCL->($cbuilder,
                    exe_file => $exe_file,
                    extra_linker_flags => $LIBS,
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

    my %extra_libs = (profile=>["-lrt"],p5=>[split(' ',ldopts)],native=>["-lm"],s1p=>["-ldl"]);
    mkdir(catdir($BUILDDIR,'lib'));
    for my $module (@MODULES) {
        my @objects = @{$self->rscan_dir("$BUILDDIR/$module/src",qr/\.o$/)};
        # FIXME libpath
        my $libpath = $self->notes('lib_path');
        my $libfile = "$BUILDDIR/lib/libsmop-$module$LIBEXT";
        if (!$self->up_to_date(\@objects, $libfile)) {
            $LD->($cbuilder,
                module_name => 'smop-' . $module,
                ($^O =~ /darwin/)?(extra_linker_flags => "-install_name $libpath"):(),
                objects => [@{$extra_libs{$module}//[]},@objects],
                lib_file => $libfile
            );
        }
    }

}
sub ACTION_compile_xscode {
    my $self = shift;
    $self->build_SMOP_xs_module('Embed');
    $self->build_SMOP_xs_module('Interoperability');
}

sub build_SMOP_xs_module {
    my ($self,$name) = @_;
    my $cbuilder = $self->cbuilder;

    my $archdir = catdir( $self->blib, 'arch', 'auto', 'SMOP',$name);
    make_path( $archdir ) unless -d $archdir;
    make_path(catdir("build","SMOP"));

    my $ext = sub {catfile("build","SMOP",$name.$_[0])};

    print STDERR "\n** Preparing XS code\n";
    my $xsfile = catfile("$name.xs");
    my $cfile = $ext->(".c");

    $self->add_to_cleanup($cfile); ## FIXME - cargo culted FIXME
    if (!$self->up_to_date($xsfile, $cfile)) {
        ExtUtils::ParseXS::process_file( filename   => $xsfile,
                                         prototypes => 0,
                                         output     => $cfile);
    }

    my $ofile = $ext->(".o");
    $self->add_to_cleanup($ofile); ## FIXME - cargo culted FIXME
    if (!$self->up_to_date($cfile, $ofile)) {
        $cbuilder->compile( source               => $cfile,
                            include_dirs         => [ smop_include_dirs() ],
                            object_file          => $ofile);
    }

#    # Create .bs bootstrap file, needed by Dynaloader.
    my $bs_file = catfile( $archdir, "$name.bs" );
    if ( !$self->up_to_date( $ofile, $bs_file ) ) {
        ExtUtils::Mkbootstrap::Mkbootstrap($bs_file);
        if ( !-f $bs_file ) {
            # Create file in case Mkbootstrap didn't do anything.
            open( my $fh, '>', $bs_file ) or confess "Can't open $bs_file: $!";
        }
        utime( (time) x 2, $bs_file );    # touch
    }

    my $objects = [$ofile];
    my $lib_file = catfile( $archdir, "$name.$Config{dlext}" );
    if ( !$self->up_to_date( [ @$objects ], $lib_file ) ) {
        say STDERR "making $lib_file";
        $cbuilder->link(
                        module_name => "SMOP::$name",
                        extra_compiler_flags => ccopts,
#                        extra_linker_flags => '-Lbtparse/src -lbtparse ',
                        objects     => $objects,
                        lib_file    => $lib_file,
                       );
    }
}

sub ACTION_test {
    my $self = shift;

    $self->dispatch("code");
    my $path = catdir($BUILDDIR,'lib');
    if ($^O =~ /darwin/i) {
        $ENV{DYLD_LIBRARY_PATH} = $path;
    }
    if ($^O =~ /(freebsd|solaris|linux)/i) {
        $ENV{LD_LIBRARY_PATH} = $path;
    }

    my $cflags = join(':',smop_include_flags(),smop_lib_flags());
    my $harness = TAP::Harness->new({ exec=>sub {
        my ($harness,$file) = @_;
        if ($file =~ /\.m0ld$/) {
            ["mildew","-F","m0ld",'++BACKEND','--no-wrap-in-block','--no-setting','--cflags',$cflags,'--ld-library-path','build/lib','++/BACKEND',$file];
        } else {
            [$file];
        }
    }});
    die "Some tests failed.\n" unless $harness->runtests(
        glob("$BUILDDIR/t/*"),
        glob("*/t/*.m0ld")
    )->all_passed;
}


1;
