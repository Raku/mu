#!/usr/bin/perl
use warnings;
use strict;

while (<>) {
  last if /^initSyms = /;
}

my %assoc = (
    spre  => 'special',
    pre   => 'prefix',
    post  => 'postfix',
    left  =>  'left-associative infix',
    right => 'right-associative infix',
    non   =>   'non-associative infix',
    list  => 'list infix',
    chain => 'chaining infix',
);

my @ops;

while (<>) {
  last unless /^\\\\n[^"]/;
  chomp;

  my (undef, $ret, $assoc, $name, $args) = split ' ', $_, 5;

  $name =~ s/\\(.)/$1/g;

  $args =~ m/\((.*)\)/ or die "Bad args spec";
  $args = $1;

  my @args = split /, *| +/, $args;

  if (exists $assoc{$assoc}) {
	$assoc = $assoc{$assoc};
  } else {
	$assoc = "FIXME: $assoc";
  }

  push @ops, {name=>$name, ret=>$ret, assoc=>$assoc, args=>[@args], line=>$.}
}

print '<?xml version="1.1" encoding="UTF-8" ?><html><body><table border="1">';



foreach my $op (sort 
		 {
		   $a->{name} cmp $b->{name} or
			 $a->{assoc} cmp $b->{assoc} or
			   @{$a->{args}} <=> @{$b->{args}}} @ops
	    ) {

  my $example;
  
  local $_=$op->{assoc};
  my $name = $op->{name};
  my $a0 = $op->{args}[0];
  my $a1 = $op->{args}[1];
  my $arity = 0+@{$op->{args}};

  if (/prefix/) {
	if (($arity>1 and $a0 =~ s/:$//) or
	    $arity==1) {
	  $example = "\$$a0.$name(".join(', ', map {"\$$_"} @{$op->{args}}[1..$arity-1]).")<br />";
	}
	$example .= "$name(". join(', ', map {$a=$_; $a=~s/:$//; '$'.$a} @{$op->{args}}) .")";
  } elsif (/(list|chaining) infix/) {
	$example = "\$$a0 $name \$$a0 $name \$$a0";
  } elsif (/left-associative/ and $arity==2) {
	$example = "(\$$a0 $name \$$a1) $name \$$a1";
  } elsif (/left-associative/ and $arity==2) {
	$example = "HUH: left-associative with arity $arity";
  } elsif (/right-associative/) {
	$example = "\$$a0 $name (\$$a0 $name \$$a1)";
  } elsif (/non-associative/) {
	$example = "\$$a0 $name \$$a1";
  } elsif (/postfix/ and $arity == 1) {
	$example = "\$$a0$name";
  } elsif (/special/ and $arity == 1) {
	$example = "$name\$$a0 # ???";
  } else {
	$example = "$_ ($arity)";
  }

  print "   <tr>";
  print "<td><tt>$example</tt></td>";
  print "<td>$op->{assoc}</td>";
  print "<td>$op->{ret}</td>";
  print "<td><tt>$op->{name}</tt></td>";
  print "<td>(</td>";
  print "<td><table border='1' width='100%'><tr>";
  foreach my $arg (@{$op->{args}}) {
	print "<td>$arg</td>";
  }
  print "</td></tr></table>";
  print "<td>)</td>";
#  print "<td>Prim.hs line $op->{line}</td>";
  print "</tr>\n";
}

print "</table></body></html>";
