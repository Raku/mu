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

print <<"__TOP__";
<!DOCTYPE html 
 PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN'
 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>
 <head>
  <link rel='stylesheet' href='testgraph.css' />
  <title>testgraph.pl @{[gmtime().'']}</title>
 </head>
 <body>
__TOP__

print "   <pre><tt>", join("\n", $data->{build_info}), "</tt></pre>\n";

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

	  my $title = ($test->{line} || '') . "\n" . ($test->{diag} || '');

	  print STDERR "pre:  $title\n";

	  $title =~ s/\cM//g;
	  $title =~ s/\cJ+$//g;
	  $title =~ s/^\cJ+//g;
	  $title =~ s/&/&amp;/g;
	  $title =~ s/</&lt;/g;
	  $title =~ s/>/&gt;/g;
	  $title =~ s!\cJ!<br />!g;
	  $title =~ s/([^-&<>\/().#A-Za-z0-9 ])/sprintf '&#x%X;', ord $1/eg;

	  print STDERR "post: $title\n";


	  my $case_link;
	  ($test->{pos} || '') =~ /^(.*?) at line (\d+), column \d+/;
	  my ($t, $line) = ($1, $2);
	  if (defined $line) {
		$case_link = "$linkto#line_$line";
	  } else {
		$case_link = $linkto;
	  }
	  
	  if ($i and $i % $per_row == 0) {
		print "</tr></table><table width='100%'><tr>\n";
	  }
	  
#	  print "<td class='test $class' title='$title'>$title</td>";
	  print " <td class='test $class'><a href='$case_link'>&nbsp;<div>$title</div></a></td>\n";
	  print STDERR " <td class='test $class'><a href='$case_link'>&nbsp;<div>$title</div></a></td>\n";
#	  print " <td class='test $class' title='$title'>&nbsp;</td>\n";

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
