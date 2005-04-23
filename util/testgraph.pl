#!/usr/bin/perl

use warnings;
use strict;

use YAML qw/Load Dump/;
use Getopt::Long;
use Test::TAP::HTMLMatrix;
use Test::TAP::Model::Visual;


GetOptions \our %Config, qw(inlinecss|e cssfile|c=s help|h);
$Config{cssfile} ||= My::HTMLMatrix->css_file();
usage() if $Config{help};

my $yamlfile = shift || 'tests.yml';

open(my $yamlfh, '<', $yamlfile) or die "Couldn't open $yamlfile for reading: $!";
binmode $yamlfh, ":utf8" or die "binmode: $!";
local $/=undef;

my $data = Load(<$yamlfh>);
undef $yamlfh;

my $tap = My::Model->new_with_struct(delete $data->{meat});
my $v = My::HTMLMatrix->new($tap, Dump($data));
inline_css($v) if $Config{inlinecss};

binmode STDOUT, ":utf8" or die "binmode: $!";
print "$v";

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

sub inline_css { # horrible hack.
    local $/;
    open my $fh, $Config{cssfile} or die "open:$Config{cssfile}:$!";
    my $css = <$fh>;
    $_[0] =~ s/^\s*<link[^\n]*css[^\n]*\r?\n/<style><!--\n$css\n--><\/style>/sm;
}

{
	# these subclass override some defaults
	package My::HTMLMatrix;
	use base qw/Test::TAP::HTMLMatrix/;

	use File::Basename;
	use File::Copy;
	use File::Spec;

	sub css_uri {
		my $self = shift;

		my $path = $main::Config{cssfile};
		
		if (File::Spec->file_name_is_absolute($path)){
			my $new = File::Spec->catfile(qw/util testgraph.css/);
			warn "renaming $new to $new.old";
			rename($new, "$new.old"); # move the old one
			copy($path, $new); # put it where it used to be
			return "util/testgraph.css"; # this is a URI, not a path
		}
		
		return $path;
	}
	
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
