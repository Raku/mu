#!/usr/bin/perl

# refresh-smoked-synopses - rerun smartlinks.pl on currently held smokes
#                           in case synopses have changed

use strict;
use warnings;

use File::Find;

use constant {
  BASEDIR     => "/var/www/iblech/stuff/pugs-smokes/",
  SMARTLINKS  => "/var/www/iblech/stuff/pugs/util/smartlinks.pl",
  PUGS_SPEC   => "/var/www/iblech/stuff/pugs-smokes/spec",
};

chdir BASEDIR or die "Couldn't chdir into \"@{[ BASEDIR ]}\": $!\n";

my $cmd = PUGS_SPEC . '/update';
my $result = `$cmd`;
if($?) {
  die "Couldn't refresh synopses: $cmd exited with $?";
}
if($result =~ m/No changes/) {
  print "Synopses haven't changed.\n";
  exit 0;
}

foreach my $yml_file (glob "pugs-smoke-*.yml") {

  my $syn_dir = $yml_file;
  $syn_dir =~ s/\.yml$/-synopses/;

  my @t_files = ();
  my $wanted = sub {
    if(m/\.t$/) {
      push @t_files, $File::Find::name;
    }
  };
  find($wanted, "$syn_dir/t");

  system(SMARTLINKS, '--test-res', $yml_file,
                     '--out-dir', $syn_dir,
                     '--syn-dir', PUGS_SPEC,
                     @t_files)
    or warn "Couldn't update smoked synopses for $yml_file";
}

