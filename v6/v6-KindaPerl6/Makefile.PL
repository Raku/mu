# Do:
#    perl Makefile.PL
#    make help
# to see help specific to this build system

use strict;
use ExtUtils::MakeMaker;

my %PM;

for my $lib (qw(mp6 kp6)) {
    my @lib_pm = glob "lib-kp6-$lib-p5/KindaPerl6/*.pm lib-kp6-$lib-p5/KindaPerl6/*/*.pm lib-kp6-$lib-p5/KindaPerl6/*/*/*.pm";

    for my $key (@lib_pm) {
        my $value = $key;

        $value =~ s[^lib-kp6-$lib-p5/KindaPerl6/][\$\(INST_LIBDIR\)/KindaPerl6/kp6-$lib-p5/lib/KindaPerl6/];

        $PM{$key} = $value;
    }

    my @mod_pm = glob "lib-modules-kp6-$lib-p5/*.pm lib-modules-kp6-$lib-p5/*/*.pm lib-modules-kp6-$lib-p5/*/*/*.pm";

    for my $key (@mod_pm) {
        my $value = $key;

        $value =~ s[^lib-modules-kp6-$lib-p5/][\$\(INST_LIBDIR\)/KindaPerl6/kp6-$lib-p5/mod/];

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
              Data::Dump::Streamer  0
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

# Voodoo magic specific to building KindaPerl6:
package MY;

sub test
{
    #my $self = shift;
    #my $inherited = $self->SUPER::test(@_);

<<'END';
test: all
	$(PERL) script/run_tests_kp6_mp6.pl

test_grammar: all
	$(PERL) script/run_tests_kp6_mp6.pl --section=grammar

test_io: all
	$(PERL) script/run_tests_kp6_mp6.pl --section=io

test_math: all
	$(PERL) script/run_tests_kp6_mp6.pl --section=math

test_base: all
	$(PERL) script/run_tests_kp6_mp6.pl --section=''
END
}

# FIXME: This uses patsubst and wildcard which are gnuisms
sub constants
{
    my $self = shift;
    my $inherited = $self->SUPER::constants(@_);

$inherited . "\n" .
<<'END'
# Custom v6-KindaPerl6 constants:
kp6_mp6_OBJ = $(patsubst src/%,compiled/perl5/lib/%,$(wildcard src/KindaPerl6/Grammar/*.pm src/KindaPerl6/Grammar.pm src/KindaPerl6/Traverse.pm src/KindaPerl6/Ast.pm src/KindaPerl6/Visitor/*.pm src/KindaPerl6/Runtime/Perl5/*.pm src/KindaPerl6/Runtime/Perl6/*.pm src/KindaPerl6/Test/*.pm ))

kp6_kp6_OBJ = $(patsubst src/%,lib-kp6-kp6-p5/%,$(wildcard src/KindaPerl6/Grammar/*.pm src/KindaPerl6/Grammar.pm src/KindaPerl6/Traverse.pm src/KindaPerl6/Ast.pm src/KindaPerl6/Visitor/*.pm src/KindaPerl6/Runtime/Perl5/*.pm src/KindaPerl6/Runtime/Perl6/*.pm src/KindaPerl6/Test/*.pm ))

kp6_mp6_modules_OBJ = $(patsubst src-modules/%,lib-modules-kp6-mp6-p5/%,$(wildcard src-modules/*.pm))

kp6_kp6_modules_OBJ = $(patsubst src-modules/%,lib-modules-kp6-kp6-p5/%,$(wildcard src-modules/*.pm))
# End custom v6-KindaPerl6 constants
END
}

# FIXME: top_targets shoul rightly just contain `all', putting all
# this other stuff here is a nasty hack!
sub top_targets
{
    my $self = shift;
    my $inherited = $self->SUPER::top_targets(@_);

    # Show relevant help on `make help'
    $inherited =~ s/perldoc ExtUtils::MakeMaker/perldoc Makefile.PL/;

    # Add the kp6 targets
    $inherited =~ s/^(all (.*))/$1 \$(kp6_mp6_OBJ) \$(kp6_kp6_OBJ) \$(kp6_mp6_modules_OBJ) \$(kp6_kp6_modules_OBJ) kp6-kp6.pl/m;

    # Add kp6_mp6 only targets, not pure_all or manifypods
    $inherited .= "\nkp6_mp6 :: \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ)\n\t\$(NOECHO) \$(NOOP)\n";

    return $inherited . "\n" .
<<'END';
# v6-KindaPerl6 specific targets:
#all: $(kp6_mp6_OBJ) $(kp6_kp6_OBJ) $(kp6_mp6_modules_OBJ) $(kp6_kp6_modules_OBJ) kp6-kp6.pl
#kp6_mp6: $(kp6_mp6_OBJ) $(kp6_mp6_modules_OBJ)


compiled/perl5/lib/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	@echo COMPILE $< $@
	@$(PERL) script/kp6 < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi
lib-modules-kp6-mp6-p5/%.pm: src-modules/%.pm
	@echo COMPILE $< $@
	@$(PERL) script/kp6 < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

lib-modules-kp6-kp6-p5/%.pm: src-modules/%.pm
	@echo COMPILE $< $@
	@$(PERL) script/kp6 -r kp6 < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi


lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	$(PERL) script/kp6 -r kp6 < $< | perltidy -pro=util/perltidyrc > $@


compiled/perl5/lib/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

compiled/perl5/lib/%.pm: src/%.pm
	$(PERL) script/mp6.pl < $< | perltidy -pro=util/perltidyrc > $@

lib-kp6-kp6-p5/%.pm: src/%.pm
	$(PERL) script/kp6 -r kp6 < $< | perltidy -pro=util/perltidyrc > $@

kp6-kp6.pl: kp6.p6
	$(PERL) script/kp6 -r kp6 < $< | perltidy -pro=util/perltidyrc > temp.pl ; \
		res=$$? ; \
		if [ $$res = 0 -a -s temp.pl ] ; then \
			cp temp.pl $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi; \
	    rm temp.pl

forcerecompile:
	find src/KindaPerl6/ -name "*.pm" | grep -v \.svn | xargs touch

# end v6-KindaPerl6 specific targets
END
}

__END__

=head1 NAME

Makefile.PL - v6::KindaPerl6 build system

=head1 SYNOPSIS

    perl Makefile.PL

=head1 TARGETS

The following targets are supported in addition to the usual
ExtUtils::MakeMaker targets.

=over

=item forcerecompile

touches all the .pm files in src/KindaPerl6, this forces recompilation
on the next I<make all>.

=item kp6_mp6

Compile only kp6-mp6 stuff, leave out kp6-kp6, much faster for development

=item test_grammar

Run grammar tests

=item test_io

Run IO tests

=item test_math

Run math tests

=item test_base

Run base tests

=back

=cut
