#!/usr/bin/perl
use warnings;
use strict;
use YAML 'Load';

my $yamlfile = shift || 'tests.yaml';

#print "Loading $yamlfile\n";

open(my $yamlfh, '<:utf8', $yamlfile) or die "Couldn't open $yamlfile for reading: $!";
local $/=undef;

my $data = Load(<$yamlfh>);
undef $yamlfh;
#Dump($data);

print "<html><head><link rel='stylesheet' href='testgraph.css'><title>testgraph.pl ".gmtime()."</title></head><body>";

print "<tt><pre>", join("\n", $data->{build_info}), "</pre></tt>";

print "<table>";

foreach my $testfile (@{$data->{test_cases}}) {
  print "<tr>";
  print " <td>", $testfile->{file}, "</td>";
  print " <td>", $testfile->{result}, "</td>";
  if (!@{$testfile->{subtests}}) {
	print " <td>No subtests -- parse failure?</td>";
  } else {
	print "<td><table width='100%'><tr>";
	my $i=0;
	foreach my $test (@{$testfile->{subtests}}) {
	  my $class = t_to_class($test);
	  my $title = $test->{line};
	  $title =~ s/([^#A-Za-z0-9 ])/sprintf '&#x%x;', ord $1/eg;

	  if ($i % 50 == 0) {
		print "</tr><tr>";
	  }

#	  print "<td class='test $class' title='$title'>$title</td>";
	  print "<td class='test $class' title='$title'>&nbsp;</td>";
	  $i++;
	}
	print "</tr></table></td>";
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
