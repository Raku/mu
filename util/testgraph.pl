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

print "<html><head><link rel='stylesheet' href='testgraph.css' /><title>testgraph.pl ".gmtime()."</title></head><body>\n";

print "<tt><pre>", join("\n", $data->{build_info}), "</pre></tt>\n";

print "<table>";

foreach my $testfile (sort {$a->{file} cmp $b->{file}}
					  @{$data->{test_cases}}) {
  print "<tr>\n";
  print " <td>", $testfile->{file}, "</td>\n";
  print " <td>", $testfile->{result}, "</td>\n";
  if (!@{$testfile->{subtests}}) {
	print " <td>No subtests -- parse failure?</td>\n";
  } else {
	print "<td><table width='100%'><tr>\n";
	my ($i, $good)=(0, 0);
	foreach my $test (@{$testfile->{subtests}}) {
	  my $class = t_to_class($test);
	  my $title = ($test->{line} || '') . ($test->{diag} || '');
	  
	  $title =~ s/\cM//g;
	  $title =~ s/\cJ$//g;
	  $title =~ s/([^-().#A-Za-z0-9 ])/sprintf '&#x%X;', ord $1/eg;
	  
	  if ($i and $i % 50 == 0) {
		print "</tr><tr>\n";
	  }
	  
#	  print "<td class='test $class' title='$title'>$title</td>";
	  print " <td class='test $class' title='$title'>&nbsp;</td>\n";
	  if ($class ne 'nottest') {
		$i++;
		$good++ if $class =~ /good/;
	  }
	}
	print "</tr></table></td>\n";
	my $pct = $good/$i;
	my $color = sprintf '#%02x%02x%02x', 0xFF*(1-$pct), 0xFF*$pct, 0;
	print " <td style='background-color: $color'>", sprintf('%.2f%%', $pct*100), "</td>";
  }
  print "</tr>\n";
}

print "</table></body></html>\n";

sub t_to_class {
  my $t=shift;
  my $p;
  my $todo;
  local $_ = $t->{line};
  
  return 'nottest' unless $t->{type} and $t->{type} eq 'test';
  
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
