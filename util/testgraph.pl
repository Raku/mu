#!/usr/bin/perl
use warnings;
use strict;
use YAML 'Load';

my $yamlfile = shift || 'tests.yml';

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
  my $linkto = $testfile->{file};
  $linkto =~ s!^t/!t_index/t/!;
  $linkto =~ s!\.t$!.html!;

  print "<tr>\n";
  print " <td><a href='$linkto'>", $testfile->{file}, "</a></td>\n";
  print " <td>", $testfile->{result}, "</td>\n";
  if (!@{$testfile->{subtests}}) {
	print " <td>No subtests -- parse failure?</td>\n";
  } else {
	print "<td><table width='100%'><tr>\n";
	my ($i, $good)=(0, 0);

	my $rows = int(.75 + @{$testfile->{subtests}} / 50) || 1;
	my $per_row = int(.75 + @{$testfile->{subtests}} / $rows);
	
	foreach my $test (@{$testfile->{subtests}}) {
	  my $class = t_to_class($test);
	  my $title = ($test->{line} || '') . ($test->{diag} || '');
	  
	  $title =~ s/\cM//g;
	  $title =~ s/\cJ$//g;
	  $title =~ s/([^-().#A-Za-z0-9 ])/sprintf '&#x%X;', ord $1/eg;

	  $test->{pos} =~ /^(.*?) at line (\d+), column \d+/;
	  my ($t, $line) = ($1, $2);
	  my $case_link = "$linkto#line_$line";
	  
	  if ($i and $i % $per_row == 0) {
		print "</tr></table><table width='100%'><tr>\n";
	  }
	  
#	  print "<td class='test $class' title='$title'>$title</td>";
	  print " <td class='test $class' title='$title'><a href='$case_link' style='display: block; text-decoration: none'>&nbsp;</a></td>\n";
	  #print " <td class='test $class' title='$title'>&nbsp;</td>\n";
	  if ($class ne 'nottest') {
		$i++;
		$good++ if $class =~ /good/;
	  }
	}
	print "</tr></table></td>\n";
	if ($i) {
	  my $pct = $good/$i;
	  my $color = sprintf '#%02x%02x%02x', 0xFF*(1-$pct), 0xFF*$pct, 0;
	  print " <td style='background-color: $color'>", sprintf('%.2f%%', $pct*100), "</td>\n";
	} else {
	  print " <td style='background-color: #7f7f7f'>???</td>\n"
	}
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
