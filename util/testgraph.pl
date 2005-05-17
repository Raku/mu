#!/usr/bin/perl

use warnings;
use strict;

use YAML qw/Load Dump/;
use Getopt::Long;
use Test::TAP::HTMLMatrix;
use Test::TAP::Model::Visual;


GetOptions \our %Config, qw(inlinecss|e cssfile|c=s help|h);
$Config{cssfile} ||= Test::TAP::HTMLMatrix->css_file();
usage() if $Config{help};

use Data::Dumper;
warn Dumper(\%Config);

my $yamlfile = shift || 'tests.yml';

open(my $yamlfh, '<', $yamlfile) or die "Couldn't open $yamlfile for reading: $!";
binmode $yamlfh, ":utf8" or die "binmode: $!";
local $/=undef;

my $data = Load(<$yamlfh>);
undef $yamlfh;

my $tap = My::Model->new_with_struct(delete $data->{meat});
my $v = Test::TAP::HTMLMatrix->new($tap, Dump($data));
$v->has_inline_css($Config{inlinecss});

my $fh;
if (my $out = shift) {
    open $fh, '>', $out or die $!;
}
else {
    $fh = \*STDOUT;
}
binmode $fh, ":utf8" or die "binmode: $!";
print $fh "$v";
close $fh;

sub usage {
  print <<"USAGE";
usage: $0 [OPTIONS] > output_file.html

Generates an HTML summary of a YAML test run. Options:

   --inlinecss, -e      inline css in HTML header (for broken webservers)
   --cssfile,  -c FILE  location of css. [default: $Config{cssfile}]

See also:
   util/yaml_harness.pl  - produce the data for this tool
   util/catalog_tests.pl - produce cross-linkable tests
   util/run-smome.pl     - automate the smoke process

USAGE
  exit 0;
}

{
	package My::Model;
	use base qw/Test::TAP::Model::Visual/;
	sub file_class { "My::File" }
	
	package My::File;
	use base qw/Test::TAP::Model::File::Visual/;
	sub subtest_class { "My::Subtest" }
	sub link {
		my $self = shift;
		my $link = $self->SUPER::link;
		$link =~ s/\.t$/.html/;
		$link;
	}

	package My::Subtest;
	use base qw/Test::TAP::Model::Subtest::Visual/;
	sub link {
		my $self = shift;
		my $link = $self->SUPER::link;
		$link =~ s/\.t(?=#line|$)/.html/;
		$link;
	}
}
