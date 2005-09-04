###
### Parser.pm
###
### Originally from Higher-Order Perl by Mark Dominus, published by Morgan
### Kaufmann Publishers, Copyright 2005 by Elsevier Inc
###
### Ported to Perl6 by Dan Brook
###

## Chapter 8 section 3

## XXX - Exporting still doesn't work quite right.
# package Parser;

# use lib '../Chap6';
use Stream;# :all;

# use base Exporter;
# @EXPORT_OK = qw(parser nothing End_of_Input lookfor
#                 alternate concatenate star list_of 
#                 operator T 
#                 error action test);
# %EXPORT_TAGS = ('all' => \@EXPORT_OK);

## Chapter 8 section 3.1

sub nothing($input) {
  return(undef, $input);
}

sub End_of_Input($input) is exported(:all) {
  return defined($input) ?? () :: (undef, undef);
}

## Chapter 8 section 3.1

## XXX - the sub(Siglet) syntax works better
# sub parser (Code $f) { $f }

## Chapter 8 section 3.1

sub lookfor($wanted, Code ?$value = { $^v.[1] }, ?$u) is exported(:all) {
  ## XXX - orig: $wanted = [$wanted] unless ref $wanted;
  my $wp := $wanted.isa(Array) ?? $wanted :: [$wanted];

  return sub($input) {
    return
      unless defined $input;
    my $next = head($input);
    
    for 0 .. $wp.end {
      next
        unless defined $wp.[$_];
      return
        unless ~$wp.[$_] eq ~$next.[$_];
    }
    
    return $value.($next, $u), tail($input);
  };
}

## Chapter 8 section 3.2

sub concatenate(Array *@p) is exported(:all) {
  return &nothing
    if +@p == 0;
  return @p[0]
    if +@p == 1;

  return sub($input) {

    my($v, @values);
    for @p {
      ($v, $input) = $_.($input)
        or return;
      @values.push($v);
    }
    ## XXX - @values != \@values
    return \@values, $input;
  };
}

## Chapter 8 section 3.2

sub alternate(Array *@p) is exported(:all) {
  return -> { }
    if +@p == 0;
  return @p[0]
    if +@p == 1;

  return sub($input) {
    my($v, $newinput);
    for @p {
      ($v, $newinput) = $_.($input);
      ## XXX - this is teh b0rk: (($v, $newinput) = $_->($input))
      return($v, $newinput)
        if ?$v;
    }
    return;
  };
}

1;
