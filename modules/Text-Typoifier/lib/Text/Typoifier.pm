module Text::Typoifier-0.04a; # XXX -- "a" allowed in version?

sub transform(Str $text, Int ?$errorrate = 5) is export {
  my $done = 0;
  if rand 10 < $errorrate {
    for 0..100 {
      my $text2 = _transform($text);
      if $text2 ne $text {
	return $text2;
      }
    }
  }

  return $text;
}


sub _transform(Str $text) {
  given int rand 4 {
    when 0 { transpose2 .($text) }
    when 1 { stickyshift.($text) }
    when 2 { double     .($text) }
    when 3 { deletion   .($text) }
  }
}

sub transpose(Str $text) {
  # this transposes any two characters. very unrealistic.
  my $length     = length $text;
  my $randomChar = int rand($length - 1);
  # XXX -- this seems to be *very* inefficient
  $text ~~ m/(.*)(.**{$randomChar})(.)(.)(.*)/;
  return $1 ~ $2 ~ $4 ~ $3 ~ $5;
}

sub double(Str $text) {
  my @sa = split '', $text;
  my $randomChar = pick(0..length $text); # XXX -- correct?
  if @sa[$randomChar] ~~ m/<[A-Za-z]>/ {
    splice @sa, $randomChar, 0, @sa[$randomChar];
    return join "", @sa;
  }
  return $text;
}

sub deletion(Str $text) {
  my @sa = split '', $text;
  my $randomChar = pick(0..length $text); # XXX -- correct?
  splice @sa, $randomChar, 1;
  return join "", @sa;
}

sub stickyshift(Str $text) {
  # this acts like a sticky shift key ie. TEsting
  my @sa = split '', $text;
  my $randomChar = pick(0..length $text);
  if $text ~~ m/<[A-Z]><[a-zA-Z]>/ {
   my $done = 0;
   while $done == 0 {
    if @sa[$randomChar] ~~ m/<[A-Z]>/ {
      @sa[$randomChar+1] = uc @sa[$randomChar+1];
      $done = 1;
    }
    $randomChar = int rand(@sa - 1 - 1);
   }
   return join "", @sa;
  }
  return $text;
}

sub transpose2(Str $text) {
  # this transposes two characters, but only if they are lowercase
  # and also [a-z]
  my @sa = split '', $text;
  my $randomChar = int rand(@sa - 1 - 1);
  if @sa[$randomChar] ~~ m/<[a-z\ ]>/ and @sa[$randomChar] ~~ m/<[a-z\ ]>/ {
    (@sa[$randomChar], @sa[$randomChar + 1]) =
      (@sa[$randomChar + 1], @sa[$randomChar]);	
  }
  return join "", @sa;
}

1;

=head1 NAME 

Text::Typoifier - mangles text

=head1 SYNOPSIS

   use Text::Typoifier <transform>;

   $text = transform($text);

=head1 DESCRIPTION

Text::Typoifier is used when you have a sample of text that you wish to induce
random errors in the text. I use this for a few IRC bots to lend a little extra
credibility to the bot. It's not really hard to use. 

=head1 SUBROUTINES

=head2 transform(Str $text, Int ?$errorrate = 5)
 
Pass in the text to transform, returns the transformed text.

The C<errorrate> parameter configures the percentage of errors that the module
outputs. The value must be an integer between 1 and 10. 10 means that 100% of
the time an error will be present in the text. 5 means that 50% of the time an
error will be in the text.

=head1 PORTING NOTES

While porting this module to Perl 6, I noticed that the module makes heavy use
of strings as arrays. For efficiency, those parts should get reported once
direct access of strings as arrays is specified.

=head1 REQUIRES

Perl 5

=head1 EXPORTS

C<transform>

=head1 AUTHOR

xjharding@mac.com

Port to Perl 6 by Ingo Blechschmidt (iblech@web.de)

=cut
