use YAML;

my $targets;

###############################################################################
# KP6-MP6
$targets->{'KP6-MP6'} = {
        lib         => 'compiled/perl5-kp6-mp6/lib',
        module_lib  => 'compiled/perl5-kp6-mp6/lib',
        run_test    => 'script/run_tests.pl --backend=perl5',
        make        =>
<<'END',

compiled/perl5-kp6-mp6/lib/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	@echo perl script/kp6 -Cperl5 -o $@.temp $<
	@$(PERL) script/kp6 -Cperl5 -o $@.temp $< ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			rm $@.temp ; \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

compiled/perl5-kp6-mp6/lib/%.pm: src-modules/%.pm
	# @echo perl script/kp6 -Cperl5 -o $@.temp $<
	@$(PERL) script/kp6 -Cperl5 -o $@.temp $<  ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			rm $@.temp ; \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

compiled/perl5-kp6-mp6/lib/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

compiled/perl5-kp6-mp6/lib/KindaPerl6/Runtime/Perl5V6/%.pm: src/KindaPerl6/Runtime/Perl5V6/%.pm
	$(CP) $< $@

compiled/perl5-kp6-mp6/lib/KindaPerl6.pm: src/KindaPerl6.pm
	$(CP) $< $@

# note, the files compiled by script/mp6.pl are all the files not handled
# or "claimed" by the above rules.
compiled/perl5-kp6-mp6/lib/%.pm: src/%.pm
	$(PERL) script/mp6.pl -o $@ $<

END
};

###############################################################################
# KP6-KP6
$targets->{'KP6-KP6'} = {
        lib         => "compiled/perl5-kp6-kp6/lib",
        module_lib  => "compiled/perl5-kp6-kp6/lib",
        run_test    => "script/run_tests_kp6_kp6.pl",
        make        =>
<<'END',
compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	$(PERL) script/kp6-kp6.pl -Cperl5rx < $< | perltidy -pro=$perltidyrc -o $@

compiled/perl5-kp6-kp6/lib/%.pm: src-modules/%.pm
	@echo perl script/kp6-kp6.pl -Cperl5rx $< $@
	@$(PERL) script/kp6-kp6.pl -Cperl5rx < $< | perltidy -pro=$perltidyrc -o $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			rm $@.temp ; \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/KindaPerl6.pm: src/KindaPerl6.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/%.pm: src/%.pm
	$(PERL) script/kp6-kp6.pl -Cperl5rx < $< | perltidy -pro=$perltidyrc -o $@

script/kp6-kp6.pl: src-script/kp6-kp6.pl
	@echo perl script/kp6-kp6.pl -Cperl5rx $< $@
	@$(PERL) script/kp6-kp6.pl -Cperl5rx < $< | perltidy -pro=$perltidyrc -o $@.temp ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			rm $@.temp ; \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

END
};

###############################################################################
# KP6-BOOT
$targets->{'KP6-BOOT'} = {
        lib         => "compiled/perl5-kp6-kp6/lib",
        module_lib  => "compiled/perl5-kp6-kp6/lib",
        run_test    => "script/run_tests_kp6_kp6.pl",
        make        =>
<<'END',
script/%.pl: src-script/%.pl
	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@ $<

compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@ $<

compiled/perl5-kp6-kp6/lib/%.pm: src-modules/%.pm
	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@.temp $<
	@$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@.temp $<  ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl5/Pad.pm: libkp6/KindaPerl6/Runtime/Perl5/Pad.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl5/Match.pm: lib5regex/KindaPerl6/Runtime/Perl5/Match.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@
compiled/perl5-kp6-kp6/lib/KindaPerl6/Runtime/Perl5V6/%.pm: src/KindaPerl6/Runtime/Perl5V6/%.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/KindaPerl6.pm: src/KindaPerl6.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6/lib/%.pm: src/%.pm
	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@ $<

script/kp6-kp6.pl: src-script/kp6-kp6.pl
	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@ $<

END
};

###############################################################################
# KP6-BOOT-NOREGEX
$targets->{"KP6-BOOT-NOREGEX"} = {
        lib         => "compiled/perl5-kp6-kp6-noregex/lib",
        module_lib  => "compiled/perl5-kp6-kp6-noregex/lib",
        run_test    => "script/run_tests_kp6_kp6.pl",  # TODO
        make        =>
<<'END',
compiled/perl5-kp6-kp6-noregex/lib/KindaPerl6/Runtime/Perl6/%.pm: src/KindaPerl6/Runtime/Perl6/%.pm
	@echo perl script/kp6 -Cperl5 --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5 --noperltidy -o $@ $<

compiled/perl5-kp6-kp6-noregex/lib/%.pm: src-modules/%.pm
	@echo perl script/kp6 -Cperl5 --noperltidy -o $@.temp $<
	@$(PERL) script/kp6 -Cperl5 --noperltidy -o $@.temp $<  ; \
		res=$$? ; \
		if [ $$res = 0 -a -s $@.temp ] ; then \
			mv $@.temp $@ ; \
		else \
			echo "*** Compilation failed with exit code: $$res!" ; \
			exit; \
		fi

compiled/perl5-kp6-kp6-noregex/lib/KindaPerl6/Runtime/Perl5/Pad.pm: libkp6/KindaPerl6/Runtime/Perl5/Pad.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6-noregex/lib/KindaPerl6/Runtime/Perl5/%.pm: src/KindaPerl6/Runtime/Perl5/%.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6-noregex/lib/KindaPerl6.pm: src/KindaPerl6.pm
	$(CP) $< $@

compiled/perl5-kp6-kp6-noregex/lib/%.pm: src/%.pm
	@echo perl script/kp6 -Cperl5 --noperltidy -o $@ $<
	$(PERL) script/kp6 -Cperl5 --noperltidy -o $@ $<

# TODO
# script/kp6-kp6.pl: src-script/kp6-kp6.pl
#	@echo perl script/kp6 -Cperl5rx --noperltidy -o $@ $<
#	$(PERL) script/kp6 -Cperl5rx --noperltidy -o $@ $<

END
};

###############################################################################
# KP6-LISP(*)
# there are several lisp targets
for my $lisp ( qw | sbcl clisp ecl | ) {
    # note usage of lisp in 'run_test'
    my $target = 'KP6-LISP' . (uc $lisp);

    $targets->{$target} = {
        lib         => 'compiled/cl/lib',
        module_lib  => 'compiled/cl/lib',
        run_test    => "script/run_tests.pl --backend=cl-$lisp",
        make        =>
<<'END',
# Hack to get the .lisp files copied under perl Makefile.PL && make
compiled/cl/lib/KindaPerl6/Runtime/Lisp/%.lisp: src/KindaPerl6/Runtime/Lisp/%.lisp
	$(CP) $< $@
END
    };
}

###############################################################################
# KP6-LISP  # the default lisp target
{
    my $target = 'KP6-LISP';

    $targets->{$target} = {
        lib         => 'compiled/cl/lib',
        module_lib  => 'compiled/cl/lib',
        run_test    => "script/run_tests.pl --backend=cl-sbcl",
        make        =>
<<'END',
# Hack to get the .lisp files copied under perl Makefile.PL && make
compiled/cl/lib/KindaPerl6/Runtime/Lisp/%.lisp: src/KindaPerl6/Runtime/Lisp/%.lisp
	$(CP) $< $@
END
    };
}

###############################################################################
# Write the file out.
print STDERR "targets.yml has been written.\n";

open my $out, '>', 'targets.yml' || die "Cannot write to targets.yml :$!";
print $out YAML::Dump( $targets );
close $out;

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
