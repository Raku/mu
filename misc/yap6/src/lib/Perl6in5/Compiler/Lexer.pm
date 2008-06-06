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
    my ( $input, $label, $pattern, $maketoken ) = @_;
    $maketoken ||= sub { [ $_[0] => $_[1] ] };
    my @tokens;
    my $buf = "";    # set to undef when input is exhausted
    my $split = sub { split /($pattern)/ => $_[0] };
    return sub {
        while ( 0 == @tokens && defined $buf ) {
            my $i = $input->();
            if ( ref $i ) {    # input is a token
                my ( $sep, $tok ) = $split->($buf);
                $tok = $maketoken->( $label, $tok ) if defined $tok;
                push @tokens => grep defined && $_ ne "" => $sep, $tok, $i;
                $buf = "";
                last;
            }
            $buf .= $i if defined $i;    # append new input to buffer
            my @newtoks = $split->($buf);
            while ( @newtoks > 2 || @newtoks && !defined $i ) {
                # buffer contains complete separator plus combined token
                # OR we've reached the end of input
                push @tokens => shift @newtoks;
                push @tokens => $maketoken->( $label, shift @newtoks )
                  if @newtoks;
            }
            # reassemble remaining contents of buffer
            $buf = join "" => @newtoks;
            undef $buf unless defined $i;
            @tokens = grep $_ ne "" => @tokens;
        }
        $_[0] = '' unless defined $_[0];
        return 'peek' eq $_[0] ? $tokens[0] : shift @tokens;
    };
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

sub string_lexer {
    my $text = shift;
    my @text = $text;
    return make_lexer( sub { shift @text }, @_ );
}

sub iterator_to_stream {
  my $it = shift;
  my $v = $it->();
  return unless defined $v;
  node($v, sub { iterator_to_stream($it) });
}

1;
