#!/usr/bin/perl

use strict;
use warnings;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name          ('PIL-Run');
version_from  ('lib/PIL/Run/ApiX.pm');
abstract_from ('lib/PIL/Run/ApiX.pm');
author        ('Mitchell N. Charity');
clean_files   (map fixpaths($_), 'blib6');
set_postamble ();
WritePugs     (5);


sub set_postamble {
	my $config = get_pugs_config();

	postamble(<< ".");
INST6_LIB = blib6/lib
INST6_ARCHLIB = blib6/arch
INST6_SCRIPT = blib6/script
INST6_BIN = blib6/bin
INST6_MAN1DIR = blib6/man1
INST6_MAN3DIR = blib6/man3

pure_all ::
	\$(PERLRUN) util/src_to_blib.pl

pure_site_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{privlib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{archlib} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{installbin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{installscript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installman3dir}

pure_vendor_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{privlib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{archlib} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{installbin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{installscript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installman3dir}
.
}
