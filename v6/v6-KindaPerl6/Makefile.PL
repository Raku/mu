use strict;
use warnings;
use ExtUtils::MakeMaker;

# target.yml contains the various build parameters for each build.
use YAML;
use Getopt::Long;

# Do:
#    perl Makefile.PL
#    make help
# to see help specific to this build system

=head1 NAME

Makefile.PL for KindaPerl6

=head1 SYNOPSIS

perl Makefile.PL

=head1 DESCRIPTION

This Makefile.PL produces a Makefile for usage by gnu make.

=head2 OPTIONS

=over 4

=item --manual

If --manual is specified the MAN3POD pages will be produced.  When, I inquired
on the #perl6 list, only 1 person admitted to reading the man pages... once.

This feature defaults to off

=item --target

--target specifies the target "back-end" to use.  The default is 'KP6-MP6'.
Also note, that the environmental variable "KP6_TARGET" can be set in lieu of
--target.

Current available targets are:

 KP6-BOOT
 KP6-BOOT-NOREGEX
 KP6-KP6
 KP6-LISP
 KP6-LISPCLISP
 KP6-LISPECL
 KP6-LISPSBCL
 KP6-MP6

=item --perltidy=[location]

--perltidy specifies the location of your perltidy file.  It defaults
to the environmental variable PERLTIDY or 'util/perltidyrc'.

=back

=head1 make targets

=over 4

=item make forcerecompile

forcerecompile touch all of the *.pm files, forcing on the next "make all"
command to recompile all KindaPerl6 files.

WARNING: You cannot use 'make -j' to rebuild after this.
as the make program will attempt to use files that have not yet been built.

=item make test

Run all of the tests for KindaPerl6

=item make test_grammar

Run the test files for KindaPerl6 grammar

=item make test_io

Run the test files for KindaPerl6 io

=item make test_math

Run the test files for KindaPerl6 math

=item make test_todo

Run the test files for KindaPerl6 todo

=back

=cut

my %options;

# I do not think anyone reads these, and the listing of man pages in the make
# files is very long and makes the Makefile hard to read.
# Further more, no one on #perl6 admitted to reading them more than once. :)
$options{manual} = 0;
$options{perltidy} = $ENV{PERLTIDY} || 'util/perltidyrc';

# default target is in ENV{KP6_TARGET} or will default to 'KP6-MP6'.
$options{target} = $ENV{KP6_TARGET} || 'KP6-MP6';

GetOptions(
    'manual'     => \$options{manual},
    'perltidy=s' => \$options{perltidy},
    'target=s'   => \$options{target},
);

warn "Warning perltidy file: $options{perltidy} does not exist\nContinuing with build anyway.\n"
    if !-e $options{perltidy};

print STDERR "Using backend: $options{target}\n";
print STDERR "Note: if you want to use a different backend, such as KP6-KP6 you will have to re-run Makefile.PL\nEx: perl Makefile.PL --target=KP6-KP6\n";

# start the actual programming!

our %Kp6Build = load_configuration( $options{target} );
my %PM = find_pm( \%Kp6Build );

my %extra_params;
$extra_params{MAN3PODS} = find_pods() if $options{manual};

WriteMakefile(
    NAME         => 'KindaPerl6',
    AUTHOR       => 'The Pugs Team <perl6-compiler@perl.org>',
    ABSTRACT     => 'A bootstrapping Perl 6 compiler with multiple emitter backends',
    VERSION_FROM => 'src/KindaPerl6.pm',
    EXE_FILES    => ['script/kp6'],
    PM           => \%PM,
    LICENSE      => 'perl',
    PREREQ_FATAL => 0,
    PREREQ_PM    => {

        # src/
        qw(
            Carp                  0
            Exporter              0
            Data::Dumper          0
            Perl::Tidy            0
            YAML                  0
            ),

        # script/kp6
        qw(
            Getopt::Long   0
            Pod::Usage     0
            Digest         0
            Digest::MD5    0
            )
    },
    %extra_params,
);

=head1 Code Documentation

=head2 load_configuration

Args: Pass the target that you want to build for.

Loads the configuration for each target from targets.yml, the $Kp6Build{make}
file has the $perltidyrc file replaced with $options{perltidy}.

Use util/make_targets_yml.pl to modify the targets.yml file.

=cut

sub load_configuration {    # load target configuration
    my $target  = shift;
    my $targets = YAML::LoadFile('targets.yml');
    my %Kp6Build;

    die "Unknown KP6_TARGET '$target'; cannot continue."
        unless exists $targets->{$target};

    %Kp6Build = %{ $targets->{$target} };

    # opps, YAML files do not allow for substitution.
    $Kp6Build{make} =~ s/\$perltidyrc/$options{perltidy}/g;

    return %Kp6Build;
}

=head2 find_pm

Return a list of all *.pm and *.lisp files.

=cut

sub find_pm {
    my %Kp6Build = %{ $_[0] };
    my %PM = ( 'src/KindaPerl6.pm' => '$(INST_LIBDIR)/KindaPerl6.pm' );

    my @lib_pm = glob "$Kp6Build{lib}/KindaPerl6/*.pm $Kp6Build{lib}/KindaPerl6/*/*.pm $Kp6Build{lib}/KindaPerl6/*/*/*.pm";
    push @lib_pm, glob "compiled/cl/lib/KindaPerl6/Runtime/Lisp/*lisp";
    for my $key (@lib_pm) {
        $PM{$key} = $key;
        $PM{$key} =~ s[^$Kp6Build{lib}/KindaPerl6/][\$\(INST_LIBDIR\)/KindaPerl6/$Kp6Build{lib}/KindaPerl6/];
        $PM{$key} =~ s[^compiled/cl/lib/KindaPerl6/][\$\(INST_LIBDIR\)/KindaPerl6/compiled/cl/lib/KindaPerl6/];
    }

    my @mod_pm = glob "$Kp6Build{module_lib}/*.pm $Kp6Build{module_lib}/*/*.pm $Kp6Build{module_lib}/*/*/*.pm";
    for my $key (@mod_pm) {
        $PM{$key} = $key;
        $PM{$key} =~ s[^$Kp6Build{module_lib}/][\$\(INST_LIBDIR\)/KindaPerl6/$Kp6Build{module_lib}/];
    }

    return %PM;
}

=head2 find_pods

Return a list of the pod files.  Return them in a hash
key= path/filename
value = '$(INST_MAN3DIR)/[path/filename from key]'

=cut

sub find_pods {
    my $str = join( ' ', map @$_, map { [ "src/*.$_", "src/*/*.$_", "src/*/*/*.$_", "src/*/*/*/*.$_" ] } qw(pm) );
    my %MAN3POD;

    for my $key ( glob $str ) {

        # for each perl package
        # convert src/My/Perl/Package.pm into
        # $(INST_MAN3DIR)/My::Perl::Package.3pm
        my ($value) = $key =~ m[^src/(.*)];
        ( my $name = $value ) =~ s[/][::];
        $name =~ s!\.pm$!\.3pm!;    # replace .pm with .3pm
        $MAN3POD{$key} = '$(INST_MAN3DIR)/' . $name;
    }

    return %MAN3POD;
}

# Voodoo magic specific to building KindaPerl6:
package MY;

=head2 test

Our custom tests

=cut

sub test {

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

sub _patsubst_wildcard {
    my ( $search, $replace, @globs ) = @_;
    my @files;

    for my $glob (@globs) {
        for my $file (< $glob >) {
            $file =~ s!^$search!$replace!o;
            push @files, $file;
        }
    }

    return join ' ', @files;
}

sub constants {
    my $self      = shift;
    my $inherited = $self->SUPER::constants(@_);
    $inherited .= "\n";
    $inherited .= "# Custom v6-KindaPerl6 constants:\n";

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
# the directories listed in Makefile.PL sub constants().  This is due to
# Microsoft's nmake not supporting features that gnu make supports.

# The problem that will arise is that if you add a file to a directory however,
# make will not "add" that file to the list of files to be compiled. To fix
# this you will have to rebuild the Makefile: "perl Makefile.PL"
END

        warn $warning;

        $inherited .= 'kp6_mp6_OBJ = ' . _patsubst_wildcard(
            qw |
                src/
                compiled/perl5-kp6-mp6/lib/
                src/*.pm
                src/KindaPerl6/Grammar/*.pm
                src/KindaPerl6/Grammar.pm
                src/KindaPerl6/Traverse.pm
                src/KindaPerl6/Ast.pm
                src/KindaPerl6/Visitor/*.pm
                src/KindaPerl6/Visitor/Emit/*.pm
                src/KindaPerl6/Runtime/Perl5V6/*.pm
                src/KindaPerl6/Runtime/MiniPerl6Like/*.pm
                src/KindaPerl6/Runtime/Perl5/*.pm
                src/KindaPerl6/Runtime/Perl6/*.pm
                src/KindaPerl6/Test/*.pm |
        ) . "\n";

        $inherited .= 'kp6_mp6_OBJ = ' . _patsubst_wildcard(
            qw |
                src/
                compiled/cl/lib/
                src/KindaPerl6/Runtime/Lisp/*.lisp |
        ) . "\n";
    }
    else {
        $inherited .= <<"END"
kp6_mp6_OBJ = \$(patsubst src/%,$Kp6Build{lib}/%,\$(wildcard src/*.pm src/KindaPerl6/Grammar/*.pm src/KindaPerl6/Grammar.pm src/KindaPerl6/Traverse.pm src/KindaPerl6/Ast.pm src/KindaPerl6/Visitor/*.pm src/KindaPerl6/Visitor/Emit/*.pm src/KindaPerl6/Runtime/Perl5V6/*.pm src/KindaPerl6/Runtime/MiniPerl6Like/*.pm src/KindaPerl6/Runtime/Perl5/*.pm src/KindaPerl6/Runtime/Perl6/*.pm src/KindaPerl6/Test/*.pm ))

kp6_mp6_modules_OBJ = \$(patsubst src-modules/%,$Kp6Build{module_lib}/%,\$(wildcard src-modules/*.pm))
END
    }

    $inherited .= "# End custom v6-KindaPerl6 constants\n";

    return $inherited;
}

# FIXME: top_targets should rightly just contain `all', putting all
# this other stuff here is a nasty hack!
sub top_targets {
    my $self      = shift;
    my $inherited = $self->SUPER::top_targets(@_);

    # Show relevant help on `make help'
    $inherited =~ s/perldoc ExtUtils::MakeMaker/perldoc Makefile.PL/;

    # Add the kp6 targets
    $inherited =~ s/^(all (.*))/$1 \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ) script\/kp6-kp6.pl/m;

    # Add kp6_mp6 only targets, not pure_all or manifypods
    $inherited .= "\nkp6_mp6 :: \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ)\n\t\$(NOECHO) \$(NOOP)\n";

    $inherited .= <<'END';

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

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
