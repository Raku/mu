#!/usr/bin/perl
use warnings;
use strict;
use YAML 'Load';

my $yamlfile = shift || 'tests.yaml';

#print "Loading $yamlfile\n";

open(my $yamlfh, '<:utf8', $yamlfile) or die "Couldn't open $yamlfile for reading: $!";
local $/='';

my $data = Load(<$yamlfh>);
undef $yamlfh;
#Dump $data;

print "<html><head><link rel='stylesheet' href='testgraph.css'><title>testgraph.pl ".gmtime()."</title></head><body><table>";

foreach my $testfile (@$data) {
  print "<tr>";
  print " <td>", $testfile->{file}, "</td>";
  print " <td>", $testfile->{result}, "</td>";
  if (!@{$testfile->{subtests}}) {
	print " <td>No subtests -- parse failure?</td>";
  } else {
	print "<td><table width='100%'>";
	foreach my $test (@{$testfile->{subtests}}) {
	  my $class = t_to_class($test);
	  my $title = $test->{line};
	  $title =~ s/([^#A-Za-z0-9 ])/sprintf '&#x%x;', ord $1/eg;

	  print "<td class='$class' title='$title'>&nbsp;</td>";
	}
	print "</table></td>";
  }
  print "</tr>";
}

print "</table></body></html>\n";

sub t_to_class {
  my $t=shift;
  my $p;
  my $todo;
  local $_ = $t->{line};
  
#  warn "$_";
  
  if (/^not ok/) {
	$p=0;
  } elsif (/^ok/) {
	$p=1;
  } else {
	die "$_ neither ok nor not ok?";
  }

  $todo = 0+/# TODO$/;
  

  return {
		  '00'=>'bad',
		  '10'=>'good',
		  '01'=>'todogood',
		  '11'=>'todobad'
		 }->{"$p$todo"};
}
