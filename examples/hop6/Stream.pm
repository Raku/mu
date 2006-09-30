###
### Stream.pm
###
### Originally from Higher-Order Perl by Mark Dominus, published by Morgan
### Kaufmann Publishers, Copyright 2005 by Elsevier Inc
###
### Ported to Perl6 by Dan Brook
###

## Chapter 6 section 2

#package Stream;

# use base Exporter;
# @EXPORT_OK =   <node head tail drop upto upfrom show promise
#                 filter transform merge list_to_stream cutsort
#                 iterate_function cut_loops>;
# 
# %EXPORT_TAGS = ('all' => \@EXPORT_OK);

sub Stream::import { }

sub node($h,$t) { return [$h, $t] }

sub head($s) { $s.[0] }

sub tail(Array $s) {
  $s.[1] = $s.[1].()
    if is_promise($s.[1]);
  return $s.[1];
}


sub is_promise($a) {
  return $a.isa(Code);
}

## XXX - use sub(Siglet) instead
# sub promise(Code $f) { $f }

## Chapter 6 section 2.2

sub show(Array $s is copy, Int $n? is copy) {
  my $ret = '';
  while $s && (!defined($n) || $n-- > 0) {
    ## XXX - what's $" these days?
    $ret ~= "@(head($s)) ";
    $s = tail($s);
  }
  return $ret;
}

## Chapter 6 section 2.2

## XXX - perl5 semantics are desired here
sub drop {
  my $h = head($_[0]);
  $_[0] = tail($_[0]);
  return $h;
}

1;
