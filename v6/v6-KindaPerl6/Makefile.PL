# Do:
#    perl Makefile.PL
#    make help
# to see help specific to this build system

use strict;
use ExtUtils::MakeMaker;
use YAML;

die "targets.yml does not exist" unless -e 'targets.yml';


my $perltidyrc = 'util/perltidyrc';

# Makefile.PL needs to be rerun with a different KP6_TARGET in order to build kp6-mp6, kp6-kp6, or kp6-lisp

my %Kp6Build = load_configuration( $ENV{KP6_TARGET} || 'KP6-MP6' );

my %PM;

#for my $lib (qw(mp6 kp6))
{
    my @lib_pm = glob "$Kp6Build{lib}/KindaPerl6/*.pm $Kp6Build{lib}/KindaPerl6/*/*.pm $Kp6Build{lib}/KindaPerl6/*/*/*.pm";
    push @lib_pm, glob "compiled/cl/lib/KindaPerl6/Runtime/Lisp/*lisp";

    for my $key (@lib_pm) {
        my $value = $key;

        $value =~ s[^$Kp6Build{lib}/KindaPerl6/][\$\(INST_LIBDIR\)/KindaPerl6/$Kp6Build{lib}/KindaPerl6/];
        $value =~ s[^compiled/cl/lib/KindaPerl6/][\$\(INST_LIBDIR\)/KindaPerl6/compiled/cl/lib/KindaPerl6/];

        $PM{$key} = $value;
    }

    my @mod_pm = glob "$Kp6Build{module_lib}/*.pm $Kp6Build{module_lib}/*/*.pm $Kp6Build{module_lib}/*/*/*.pm";

    for my $key (@mod_pm) {
        my $value = $key;

        $value =~ s[^$Kp6Build{module_lib}/][\$\(INST_LIBDIR\)/KindaPerl6/$Kp6Build{module_lib}/];

        $PM{$key} = $value;
    }
}

$PM{"src/KindaPerl6.pm"} = '$(INST_LIBDIR)/KindaPerl6.pm';

my %MAN3POD;

{
    my $str = join(' ', map @$_, map { [ "src/*.$_", "src/*/*.$_", "src/*/*/*.$_", "src/*/*/*/*.$_" ] } qw(pm));
    my @man = glob $str;

    for my $key (@man) {
        my $value = $key;

        my ($value) = $key =~ m[^src/(.*)];
        my $name = join '::', split m[/], $value;
        #$value =~ s[^src/][\$\(INST_MAN3DIR\)];
        $name =~ s/\.pm/\.3pm/;

        $MAN3POD{$key} = '$(INST_MAN3DIR)/' . $name;
    }
}

WriteMakefile(
    NAME          => 'KindaPerl6',
    AUTHOR        => 'The Pugs Team <perl6-compiler@perl.org>',
    ABSTRACT      => 'A bootstrapping Perl 6 compiler with multiple emitter backends',
    VERSION_FROM  => 'src/KindaPerl6.pm',
    EXE_FILES     => [ "script/kp6" ],
    PM            => \%PM,
    MAN3PODS      => \%MAN3POD,
    LICENSE       => 'perl',

    PREREQ_FATAL => 0,
    PREREQ_PM    => {
        # src/
        qw(
              Carp                  0
              Exporter              0
              Data::Dumper          0
              Perl::Tidy            0
        ),
        # script/kp6
        qw(
            Getopt::Long   0
            Pod::Usage     0
            Digest         0
            Digest::MD5    0
        )
    },
);

=head2 load_configuration

Loads the configuration for each target from targets.yml

use util/make_targets_yml.pl to write the targets.yml file.

=cut

sub load_configuration { # load target configuration
    my $targets = YAML::LoadFile('targets.yml');
    my $KP6_TARGET = $ENV{KP6_TARGET} || 'KP6-MP6';
    if ( exists $targets->{ $KP6_TARGET } ) {
        %Kp6Build = %{ $targets->{ $KP6_TARGET } };
    } else {
        die "unknown KP6_TARGET '$KP6_TARGET', can't continue";
    }
}

# Voodoo magic specific to building KindaPerl6:
package MY;

sub test
{
    #my $self = shift;
    #my $inherited = $self->SUPER::test(@_);

<<"END";
test: all
	\$(PERL) $Kp6Build{run_test}

test_grammar: all
	\$(PERL) $Kp6Build{run_test} --section=grammar

test_io: all
	\$(PERL) $Kp6Build{run_test} --section=io

test_math: all
	\$(PERL) $Kp6Build{run_test} --section=math

test_base: all
	\$(PERL) $Kp6Build{run_test} --section=''

test_todo: all
	\$(PERL) $Kp6Build{run_test} --section=todo
END
}

# while, this code does the expansion INSIDE of Makefile.PL it really
# should be done inside of the Makefile.  I just couldn't figure out a way
# to do it in nmake.

# _patsubst_wildcard
#
# _patsubst_wildcard is a subroutine to handle the gnuism usage of
#
#  $(patsub search, replace, $(wildcard globs))
#

sub _patsubst_wildcard
{
    my ( $search, $replace, @globs ) = @_;
    my @files;

    for my $glob ( @globs ) {
        for my $file ( < $glob > ) {
            $file =~ s!^$search!$replace!o;
            push @files, $file;
        }
    }

    return join ' ', @files;
}

sub constants
{
    my $self      = shift;
    my $inherited = $self->SUPER::constants(@_);
    $inherited    .= "\n";

    # FIXME: There needs to be a better way to make "nmake" include code
    # dynamically.  nmake supports an inline file (microsoft specific) however,
    # this may break other program's make programs.

    # inline file documentation for nmake
    # http://msdn2.microsoft.com/en-us/library/seaaew50(VS.71).aspx

    # Possible fixes:
    # attempt to execute make -help, nmake -help, dmake -help and see what
    # reports back as working

    # if you have a make program that supports gnuism's you can just force
    # the below line to false.
    if ( $^O eq 'MSWin32' ) {
        # nmake (Microsoft nmake doesn't support the gnuism's
        # $(patsubst search, replace, text)
        # $(wildcard glob, glob, ..)

        my $warning = <<"END";
# WARNING: You will have to rebuild Makefile when ever you add a file to
# the directories listed in Makefile.PL sub constats().  This is due to
# Microsoft's nmake not supporting features that gnu make supports.

# The problem that will arise is that if you add a file to a directory however,
# make will not "add" that file to the list of files to be compiled. To fix
# this you will have to rebuild the Makefile: "perl Makefile.PL"
END

        warn $warning;

        $inherited .= "# Custom v6-KindaPerl6 constants:\n";

        $inherited .= 'kp6_mp6_OBJ = ' . _patsubst_wildcard( qw |
                        src/
                        compiled/perl5-kp6-mp6/lib/
                        src/*.pm
                        src/KindaPerl6/Grammar/*.pm
                        src/KindaPerl6/Grammar.pm
                        src/KindaPerl6/Traverse.pm
                        src/KindaPerl6/Ast.pm
                        src/KindaPerl6/Visitor/*.pm
                        src/KindaPerl6/Visitor/Emit/*.pm
                        src/KindaPerl6/Runtime/Perl5/*.pm
                        src/KindaPerl6/Runtime/Perl6/*.pm
                        src/KindaPerl6/Test/*.pm |
        ) . "\n";

        $inherited .= 'kp6_mp6_OBJ = ' . _patsubst_wildcard( qw |
                        src/
                        compiled/cl/lib/
                        src/KindaPerl6/Runtime/Lisp/*.lisp |
        ) . "\n";

        $inherited .= "# End custom v6-KindaPerl6 constants\n";
    } else {
        $inherited .= <<"END"
# Custom v6-KindaPerl6 constants:
kp6_mp6_OBJ = \$(patsubst src/%,$Kp6Build{lib}/%,\$(wildcard src/*.pm src/KindaPerl6/Grammar/*.pm src/KindaPerl6/Grammar.pm src/KindaPerl6/Traverse.pm src/KindaPerl6/Ast.pm src/KindaPerl6/Visitor/*.pm src/KindaPerl6/Visitor/Emit/*.pm src/KindaPerl6/Runtime/Perl5V6/*.pm src/KindaPerl6/Runtime/Perl5/*.pm src/KindaPerl6/Runtime/Perl6/*.pm src/KindaPerl6/Test/*.pm ))

kp6_mp6_modules_OBJ = \$(patsubst src-modules/%,$Kp6Build{module_lib}/%,\$(wildcard src-modules/*.pm))

# End custom v6-KindaPerl6 constants
END
    }

    return $inherited;
}

# FIXME: top_targets should rightly just contain `all', putting all
# this other stuff here is a nasty hack!
sub top_targets
{
    my $self = shift;
    my $inherited = $self->SUPER::top_targets(@_);

    # Show relevant help on `make help'
    $inherited =~ s/perldoc ExtUtils::MakeMaker/perldoc Makefile.PL/;

    # Add the kp6 targets
    $inherited =~ s/^(all (.*))/$1 \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ) script\/kp6-kp6.pl/m;

    # Add kp6_mp6 only targets, not pure_all or manifypods
    $inherited .= "\nkp6_mp6 :: \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ)\n\t\$(NOECHO) \$(NOOP)\n";

    $inherited .=
<<'END';

# Touch all the .pm files in src/KindaPerl6 to force recompilation
forcerecompile:
	@echo "WARNING: You cannot use 'make -j' to rebuild after this."
	@echo "as the make program will attempt to use files that have not yet been built."
	find src/KindaPerl6/ -name "*.pm" | grep -v \.svn | xargs touch

## kp6 frontend in p6, uncomment to use
#kp6-kp6.pl: kp6.p6
#	$(PERL) script/kp6 -r kp6 -o temp.pl $< ; \
#		res=$$? ; \
#		if [ $$res = 0 -a -s temp.pl ] ; then \
#			cp temp.pl $@ ; \
#		else \
#			echo "*** Compilation failed with exit code: $$res!" ; \
#			exit; \
#		fi; \
#	    rm temp.pl

END
    $inherited .= $Kp6Build{make};
    #print $inherited;
    return $inherited;
}

__END__

