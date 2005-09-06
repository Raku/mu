#!/usr/bin/perl
use warnings;
use strict;
use IO::File;
use Data::Dumper;
$|++;

my %comitters;

while (<>) {
  next unless /^r(\d+) /;
  my ($r, $comitter, $date) = split /[|:]/, $_;
  $comitter=trim($comitter);
  $comitters{$comitter}++;
}

#foreach (sort {$comitters{$a} <=> $comitters{$b}} keys %comitters) {
#  print "$_: $comitters{$_}\n";
#}

my $authors = readauthors();
#print Dumper $authors;

foreach my $c (sort {$comitters{$b} <=> $comitters{$a}} keys %comitters) {
  print "$c: $comitters{$c}\n";
  my $author;
  my $re = qr/$c/i;
  foreach my $a (@$authors) {
    if ($a->{wholeline} =~ $re) {
      $author = $a;
      last;
    }
  }
  if (!$author) {
    # Time for some hurestics: Try matching _ as space -- Darren_Duncan for example.
    my $re = $c;
    $re =~ s/_/ /;
    $re = qr/$re/i;
    foreach my $a (@$authors) {
      if ($a->{wholeline} =~ $re) {
        $author = $a;
        last;
      }
    }
  }
  if (!$author) {
    # Guess first initial, last name.
    my ($initial, $last) = $c =~ m/^(.)(.*)/;
    $re = qr/^$initial.*$last/i;
    foreach my $a (@$authors) {
      if ($a->{wholeline} =~ $re) {
        $author = $a;
        last;
      }
    }
  }

  if (!$author) {
    warn "$c missing from AUTHORS file\n";
  }

  $author->{commitid}=$c;
  $author->{commitcount}=$comitters{$c};
  print Dumper $author;
}


sub trim {
  local $_=shift;
  s/^\s+//;
  s/\s+$//;
  return $_;
}

sub readauthors {
  my $fh = IO::File->new("AUTHORS", "<:utf8");
  my @authors;
  my (%bycpan, %bynick);

  while (<$fh>) {
    last if /^$/;
  }
  while (<$fh>) {
    chomp;
    my ($cpanid) = m/\(([A-Z]+)\)/;
    my ($nick)   = m/"([^"]+)"/;
    my $author   = {cpanid => $cpanid, nick => $nick, wholeline => $_};
    push @authors, $author;
  }
  return \@authors;
}
