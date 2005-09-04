###
### Lexer.pm
###
### Originally from Higher-Order Perl by Mark Dominus, published by Morgan
### Kaufmann Publishers, Copyright 2005 by Elsevier Inc
###
### Ported to Perl6 by Dan Brook
###

## Chapter 8 section 1.1

sub Lexer::import { }

## XXX - is exported doesn't support args atm

## Chapter 8 section 1.3

sub tokens(Code *$input, Str *$label, Str $pattern, Code ?$maketoken)
    is exported(:all) {
  ## XXX - slurpy magic doesn't work too well presently ...
  $maketoken := { [ $^tok_label, $^tok ] }
    if !defined $maketoken;
  my @tokens;
  my $buf = "";   # set to undef to when input is exhausted
  my $split := { (split rx:perl5/($pattern)/, $^str).map:{~$_} };
  return sub {
    while +@tokens == 0 && defined $buf {
      my $i = $input.();
      if $i.isa(Array) {
        my($sep, $tok) = $split.($buf);
        $tok = $maketoken.($tok, $label)
          if defined $tok;
        @tokens.push( ($sep, $tok, $i).grep:{ $_ ne "" } );
        $buf = "";
        last;
      }

      $buf ~= $i
        if defined $i;

      my @newtoks = $split.($buf);
      while +@newtoks > 2 
         || +@newtoks && !defined $i {

        @tokens.push( @newtoks.shift );
        @tokens.push( $maketoken.(@newtoks.shift, $label) )
                if +@newtoks;
      }

      $buf = [~] @newtoks;
      undefine $buf
        if !defined $i;

      @tokens.=grep:{ $_ ne "" };
    }
    # say "tokens looks like: {@tokens.perl}";
    return @tokens.shift;
  };
}


## Chapter 8 section 1.3

sub make_lexer (Code $lexer is rw, *@args) {
  ## XXX - Surely there's a more p6ish way of doing this?
  $lexer = tokens($lexer, *@$_)
    for @args;
  return $lexer;
}

1;
