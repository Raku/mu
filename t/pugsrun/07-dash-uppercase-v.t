#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-V> and C<-V:option>.


=cut

# cf. unspecced/config.t for the same list
my @config = <
  archlib archname
	bin
	exe_ext
	file_sep
	installarchlib
	installbin
	installprivlib
	installscript
	installsitearch
	installsitebin
	installsitelib
	osname
	pager
	path_sep
	perl_revision
	perl_subversion
	perl_version
	prefix
	privlib
	pugspath
	scriptdir
	sitearch
	sitebin
	sitelib
	pugs_versnum
	pugs_version
    pugs_revision
>;

plan 1+@config;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");
if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $pugs_config = run_pugs('-V');
ok( ($pugs_config ~~ rx:perl5/^This is Perl6 User's Golfing System/), "Got some config data")
  or diag $pugs_config;

# Generalize this:
for @config -> $item {
  $pugs_config = run_pugs("-V:$item");

  my $local_sep = "\t$item: %?CONFIG{$item}\n";

  todo_is( $pugs_config, $local_sep, "-V:$item works" );
};
