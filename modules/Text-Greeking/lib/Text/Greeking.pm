module Text::Greeking-0.11;

class Text::Greeking {
  class MinMax {
    has Int $.min is rw;
    has Int $.max is rw;
    method minmax() is rw {
      return new Proxy:
	FETCH => { ($.min, $.max) },
	STORE => -> $min, $max { ($.min, $.max) = ($min, $max) };
    }
  }

  # make controllable eventually.
  has @.punc   = (split '', '..........??!')         is rw;
  has @.inpunc = (split('', ',,,,,,,,,,;;:'), " --") is rw;

  has @.sources;
  has MinMax $.paragraphs will build { MinMax.new(min => 2, max =>  8) } is rw;
  has MinMax $.sentences  will build { MinMax.new(min => 2, max =>  8) } is rw;
  has MinMax $.words      will build { MinMax.new(min => 5, max => 15) } is rw;

  method BUILD($.paragraphs, $.sentences, $.words, *@sources) {
    .add_source($_) for @sources;
  }

  method add_source(Str $text is copy) {
    $text ~~ s:g/<[\n\r]>/ /;
    $text ~~ s:g/[[:punct:]]//; # XXX -- correct=
    my @words = map { lc $_ } split m/\s+/, $text;
    push @.sources, \@words;
  }

  method generate() {
    .:load_default_source unless @.sources;

    my @words = pick @.sources; # XXX -- correct?
    my ($paramin, $paramax) = map{ ($_.min, $_.max) } $.paragraphs;
    my ($sentmin, $sentmax) = map{ ($_.min, $_.max) } $.sentences;
    my ($phramin, $phramax) = map{ ($_.min, $_.max) } $.words;

    my $out;
    my $pcount = int(rand($paramax-$paramin+1)+$paramin);
    loop my $x = 0; $x < $pcount; $x++ {
      my $p;
      my $scount = int(rand($sentmax-$sentmin+1)+$sentmin);
      loop my $y = 0; $y < $scount; $y++ {
	my $s;
	my $wcount = int(rand($phramax-$phramin+1)+$phramin);
	loop my $w = 0; $w < $wcount; $w++ {
	  my $word = pick @words; # XXX -- correct?
	  $s ~= $s ?? " $word" :: ucfirst $word;
	  $s ~= (($w+1 < $wcount) && !int rand 10) ?? pick @inpunc :: '';
	  # XXX -- correct?
	}
	$s ~= pick @punc; # XXX -- correct?
	$p ~= ' ' if $p;
	$p ~= $s;
      }
      $out ~= "$p\n\n"; # assumes text.
    }

    return $out;
  }

  method :load_default_source() { .add_source(qq:to/TEXT/) }
    Lorem ipsum dolor sit amet, consectetuer adipiscing elit,
    sed diam nonummy nibh euismod tincidunt ut laoreet dolore
    magna aliquam erat volutpat. Ut wisi enim ad minim veniam,
    quis nostrud exerci tation ullamcorper suscipit lobortis
    nisl ut aliquip ex ea commodo consequat. Duis autem vel eum
    iriure dolor in hendrerit in vulputate velit esse molestie
    consequat, vel illum dolore eu feugiat nulla facilisis at
    vero eros et accumsan et iusto odio dignissim qui blandit
    praesent luptatum zzril delenit augue duis dolore te feugait
    nulla facilisi.
    Ut wisi enim ad minim veniam, quis nostrud exerci tation
    ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo
    consequat. Duis autem vel eum iriure dolor in hendrerit in
    vulputate velit esse molestie consequat, vel illum dolore eu
    feugiat nulla facilisis at vero eros et accumsan et iusto
    odio dignissim qui blandit praesent luptatum zzril delenit
    augue duis dolore te feugait nulla facilisi. Lorem ipsum
    dolor sit amet, consectetuer adipiscing elit, sed diam
    nonummy nibh euismod tincidunt ut laoreet dolore magna
    aliquam erat volutpat. 
    Duis autem vel eum iriure dolor in hendrerit in vulputate
    velit esse molestie consequat, vel illum dolore eu feugiat
    nulla facilisis at vero eros et accumsan et iusto odio
    dignissim qui blandit praesent luptatum zzril delenit augue
    duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit
    amet, consectetuer adipiscing elit, sed diam nonummy nibh
    euismod tincidunt ut laoreet dolore magna aliquam erat
    volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci
    tation ullamcorper suscipit lobortis nisl ut aliquip ex ea
    commodo consequat.
    TEXT
}

1;

__END__

=begin

=head1 NAME

Text::Greeking - a module for generating meaningless text
that creates the illusion of the finished document. 

=head1 SYNOPSIS

 use Text::Greeking;
 
 my Text::Greeking $g .= new;
 $g.paragraphs.minmax = (1,2) # min of 1 paragraph and a max of 2
 $g.sentences.minmax  = (2,5) # min of 2 sentences per paragraph and a max of 5
 $g.words.minmax      = (8,16) # min of 8 words per sentence and a max of 16
 print $g.generate; # use default Lorem Ipsum source
 
=head1 DESCRIPTION

Greeking is the use of random letters or marks to show the
overall appearance of a printed page without showing the
actual text. Greeking is used to make it easy to judge the
overall appearance of a document without being distracted by
the meaning of the text.

This is a module is for quickly generating varying
meaningless text from any source to create this illusion of
the content in systems.

This module was created to quickly give developers simulated
content to fill systems with simulated content. Instead of
static Lorem Ipsum text, by using randomly generated text
and optionally varying word sources, repetitive and
monotonous patterns that do not represent real system usage
is avoided. 

=head1 METHODS

=over

=item Text::Greeking.new

Constructor method. Returns a new instance of the class.

=item $g.add_source($text)

The class takes a body of text passed as a SCALAR and
processes it into a list of word tokens for use in
generating random filler text later.

=item $g.generate

Returns a body of random text generated from a randomly
selected source using the minimum and maximum values set by
paragraphs, sentences, and words minimum and maximum values.
If generate is called without any sources a standard Lorem
Ipsum block is used added to the sources and then used for
processing the random text.

=item $g.paragraphs.minmax = ($min,$max)

=item $g.paragraphs.min = $min

=item $g.paragraphs.max = $max

Sets the minimum and maximum number of paragraphs to
generate. Default is a minimum of 2 and a maximum of 8.

=item $g.sentences.minmax = ($min,$max)

=item $g.sentences.min = $min

=item $g.sentences.max = $max

Sets the minimum and maximum number of sentences to generate
per paragraph. Default is a minimum of 2 and a maximum of 8.

=item $g.words.minmax = ($min,$max)

=item $g.words.min = $min

=item $g.words.max = $max

Sets the minimum and maximum number of words to generate per
sentence. Default is a minimum of 5 and a maximum of 15.

=back

=head1 SEE ALSO

L<http://en.wikipedia.org/wiki/Greeking>

=head1 TO DO

=over

=item HTML output mode including random hyperlinked phrases.

=item Configurable punctuation controls.

=back

=head1 LICENSE

The software is released under the Artistic License. The
terms of the Artistic License are described at
L<http://www.perl.com/language/misc/Artistic.html>.

=head1 AUTHOR & COPYRIGHT

Except where otherwise noted, Text::Greeking is 
Copyright 2005, Timothy Appnel, tima@cpan.org. All rights 
reserved.

Copyright 2005, Ingo Blechschmidt, iblech@web.de (port to Perl 6).

=cut

=end
