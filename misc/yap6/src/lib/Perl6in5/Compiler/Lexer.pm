#
# This software is Copyright 2005 by Elsevier Inc.  You may use it
# under the terms of the license at http://perl.plover.com/hop/LICENSE.txt .
#



###
### Lexer.pm
###

## Chapter 8 section 1.1

package Perl6in5::Compiler::Lexer;
use Perl6in5::Compiler::Stream 'node';
use base "Exporter";
@EXPORT_OK = qw(make_charstream blocks records tokens iterator_to_stream
                make_lexer allinput);

%EXPORT_TAGS = ('all' => \@EXPORT_OK);

sub make_charstream {
  my $fh = shift;
  return sub { return getc($fh) };
}


## Chapter 8 section 1.1

sub records {
  my $input = shift;
  my $terminator = @_ ? shift : quotemeta($/);
  my @records;
  my @newrecs = split /($terminator)/, $input;
  while (@newrecs > 2) {
    push @records, shift(@newrecs).shift(@newrecs);
  }
  push @records, @newrecs;
  return sub {
    return shift @records;
  }
}


## Chapter 8 section 1.3

sub allinput {
  my $fh = shift;
  my @data;
  { local $/;
    $data[0] = <$fh>;
  }
  sub { return shift @data }
}
sub blocks {
  my $fh = shift;
  my $blocksize = shift || 8192;
  sub {
    return unless read $fh, my($block), $blocksize;
    return $block;
  }
}


## Chapter 8 section 1.3

sub tokens {
  my ($input, $label, $pattern, $maketoken) = @_;
  $maketoken ||= sub { [ $_[1], $_[0] ] };
  my @tokens;
  my $buf = "";   # set to undef to when input is exhausted
  my $split = sub { split /($pattern)/, $_[0] };
  sub {
    while (@tokens == 0 && defined $buf) {
      my $i = $input->();
      if (ref $i) {
        my ($sep, $tok) = $split->($buf);
        $tok = $maketoken->($tok, $label) if defined $tok;
        push @tokens, grep $_ ne "", $sep, $tok, $i;
        $buf = "";
        last;
      }

      $buf .= $i if defined $i;
      my @newtoks = $split->($buf);
      while (@newtoks > 2 
             || @newtoks && ! defined $i) {
        push @tokens, shift(@newtoks);
        push @tokens, $maketoken->(shift(@newtoks), $label) 
                if @newtoks;
      }
      $buf = join "", @newtoks;
      undef $buf if ! defined $i;
      @tokens = grep $_ ne "", @tokens;
    }
    return shift(@tokens);
  }
}


## Chapter 8 section 1.3

sub make_lexer {
  my $lexer = shift;
  while (@_) {
    my $args = shift;
    $lexer = tokens($lexer, @$args);
  }
  $lexer;
}

sub iterator_to_stream {
  my $it = shift;
  my $v = $it->();
  return unless defined $v;
  node($v, sub { iterator_to_stream($it) });
}

1;
