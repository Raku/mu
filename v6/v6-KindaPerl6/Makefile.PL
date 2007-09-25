# Do:
#    perl Makefile.PL
#    make help
# to see help specific to this build system

use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME     => 'v6-KindaPerl6',
    AUTHOR   => 'The Pugs Team <perl6-compiler@perl.org>',
    ABSTRACT => 'A bootstrapping Perl 6 compiler with multiple emitter backends',
    VERSION  => '0.01',
    LICENSE  => 'perl',

    PREREQ_FATAL => 0,
    PREREQ_PM    => { qw(
        Carp                  0
        Exporter              0
        Data::Dump::Streamer  0
        Data::Dumper          0
        Perl::Tidy            0
    ) },
);

# Voodoo magic specific to building v6-KindaPerl6:
package MY;

sub test
{
    #my $self = shift;
    #my $inherited = $self->SUPER::test(@_);

<<'END';
test: all
	$(PERL) run_tests_kp6_kp6.pl

test_grammar: all
	$(PERL) run_tests_kp6_kp6.pl --section=grammar

test_io: all
	$(PERL) run_tests_kp6_kp6.pl --section=io

test_math: all
	$(PERL) run_tests_kp6_kp6.pl --section=math

test_base: all
	$(PERL) run_tests_kp6_kp6.pl --section=''
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
kp6_mp6_OBJ = $(patsubst lib/%,lib-kp6-mp6-p5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Ast.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/*.pm lib/KindaPerl6/Test/*.pm ))

kp6_kp6_OBJ = $(patsubst lib/%,lib-kp6-kp6-p5/%,$(wildcard lib/KindaPerl6/Grammar/*.pm lib/KindaPerl6/Grammar.pm lib/KindaPerl6/Traverse.pm lib/KindaPerl6/Ast.pm lib/KindaPerl6/Visitor/*.pm lib/KindaPerl6/Runtime/Perl5/*.pm lib/KindaPerl6/Runtime/Perl6/*.pm lib/KindaPerl6/Test/*.pm ))

kp6_mp6_modules_OBJ = $(patsubst lib-modules/%,lib-modules-kp6-mp6-p5/%,$(wildcard lib-modules/*.pm))

kp6_kp6_modules_OBJ = $(patsubst lib-modules/%,lib-modules-kp6-kp6-p5/%,$(wildcard lib-modules/*.pm))
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
    $inherited =~ s/^(all .*)/$1 \$(kp6_mp6_OBJ) \$(kp6_kp6_OBJ) \$(kp6_mp6_modules_OBJ) \$(kp6_kp6_modules_OBJ) kp6-kp6.pl/m;

    # Add kp6_mp6 only targets
    $inherited .= "\nkp6-mp6: \$(kp6_mp6_OBJ) \$(kp6_mp6_modules_OBJ)\n\t\$(NOECHO) \$(NOOP)\n";

    return $inherited . "\n" .
<<'END';
# v6-KindaPerl6 specific targets:
#all: $(kp6_mp6_OBJ) $(kp6_kp6_OBJ) $(kp6_mp6_modules_OBJ) $(kp6_kp6_modules_OBJ) kp6-kp6.pl
#kp6-mp6: $(kp6_mp6_OBJ) $(kp6_mp6_modules_OBJ)


lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	@echo COMPILE $< $@
	@$(PERL) kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi
lib-modules-kp6-mp6-p5/%.pm: lib-modules/%.pm
	@echo COMPILE $< $@
	@$(PERL) kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

lib-modules-kp6-kp6-p5/%.pm: lib-modules/%.pm
	@echo COMPILE $< $@
	@KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' $(PERL) kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi


lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl6/%.pm: lib/KindaPerl6/Runtime/Perl6/%.pm
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' $(PERL) kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@


lib-kp6-mp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

lib-kp6-kp6-p5/KindaPerl6/Runtime/Perl5/%.pm: lib/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

lib-kp6-mp6-p5/%.pm: lib/%.pm
	$(PERL) mp6.pl < $< > $@

lib-kp6-kp6-p5/%.pm: lib/%.pm
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' $(PERL) kp6-mp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > $@

kp6-kp6.pl: kp6.p6
	KP6_TARGET_RUNTIME='KindaPerl6::Runtime::Perl5::KP6Runtime' $(PERL) kp6-kp6-perl5.pl < $< | perltidy -pro=util/perltidyrc > temp.pl ; \
		res=$$? ; \
		if [ $$res = 0 -a -s temp.pl ] ; then \
			cp temp.pl $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi; \
	    rm temp.pl

forcerecompile:
	find lib/KindaPerl6/ -name "*.pm" | grep -v \.svn | xargs touch

# end v6-KindaPerl6 specific targets
END
}

sub install
{
    my $self = shift;
    my $inherited = $self->SUPER::install(@_);

    $inherited =~ s/(install.*?NOOP\))/install ::\n\t\@echo "make install does not work yet"\n\tfalse\n/s;
    $inherited;
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

touches all the .pm files in lib/KindaPerl6, this forces recompilation
on the next I<make all>.

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
