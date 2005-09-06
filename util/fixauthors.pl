#!/usr/bin/perl
use warnings;
use strict;
#use CPAN;
use LWP::Simple;
use HTML::TreeBuilder;

binmode(STDOUT, ":utf8");
binmode(STDIN,  ":utf8");

my @a=<STDIN>;
my @head = @a[0..4];
@a = @a[5..$#a];

@a = 
  map {$_->[0]} 
  sort {$a->[1] cmp $a->[1]} 
  map {my $x=$_; $x=~tr/A-Za-z//cd; [$_, $x]} 
  @a;

foreach (@a) {
  next unless /\)$/;
  next unless /\((.*?)\)/;
  
  chomp;
  if ($1) {
    $_ .= "   " . cpanid2realname($1) . "\n";
  } else {
    $_ .= "\n";
  }
}

print @head;
print @a;



sub cpanid2realname {
  my $uid = shift;
  
  my $html=get("http://search.cpan.org/~$uid");
  my $tree=HTML::TreeBuilder->new_from_content($html);
  my $realname = $tree->look_down(class=>"t1");

  return "" unless $realname;
  return $realname->as_text;

  return $realname;
}
