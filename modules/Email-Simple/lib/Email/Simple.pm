module Email-Simple-1.92;
use v6;

class Email::Simple {
  my $crlf = qr/\x0a\x0d|\x0d\x0a|\x0a|\x0d/; # We are liberal in what we accept.
					      # But then, so is a six dollar whore.

  my $GROUCHY = 0;

  has $.head;
  has $.body is rw;
  has $:mycrlf;
  has $:head_hash;
  has $:header_names;
  has $:order;

=head1 NAME

Email::Simple - Simple parsing of RFC2822 message format and headers

=head1 SYNOPSIS

    my Email::Simple $mail .= new(source => $text);

    my $from_header = $mail.header("From");
    my @received = $mail.header("Received");

    $mail->header("From") = 'Simon Cozens <simon@cpan.org>';

    my $old_body = $mail.body;
    $mail->body. = "Hello world\nSimon";

    print $mail.as_string;

    # AND THAT'S ALL.

=head1 DESCRIPTION

C<Email::Simple> is the first deliverable of the "Perl Email Project", a
reaction against the complexity and increasing bugginess of the
C<Mail::*> modules. In contrast, C<Email::*> modules are meant to be
simple to use and to maintain, pared to the bone, fast, minimal in their
external dependencies, and correct.

    Can you sum up plan 9 in layman's terms?
    It does everything Unix does only less reliably - kt

=head1 METHODS

Methods are deliberately kept to a minimum. This is meant to be simple.
No, I will not add method X. This is meant to be simple. Why doesn't it
have feature Y? Because it's meant to be simple.

=head2 new

Parse an email from a scalar containing an RFC2822 formatted message,
and return an object.

=cut

method BUILD (Str $source) {
  ($.head, $.body, $.mycrlf) = .:split_head_from_body($source);
  ($:head_hash, $:order)     = .:read_headers($head);
  %:header_names = map:{ lc $^header => $^header } keys $.head_hash;
}

method :split_head_from_body(Str $source) {
  # The body is simply a sequence of characters that
  # follows the header and is separated from the header by an empty
  # line (i.e., a line with nothing preceding the CRLF).
  #  - RFC 2822, section 2.1
  if $source ~~ m/(.*?($crlf))$2(.*)/) {
      return $1, $3, $2;
  } else { # The body is, of course, optional.
      return $source, "", "\n";
  }
}

# Header fields are lines composed of a field name, followed by a colon
# (":"), followed by a field body, and terminated by CRLF.  A field
# name MUST be composed of printable US-ASCII characters (i.e.,
# characters that have values between 33 and 126, inclusive), except
# colon.  A field body may be composed of any US-ASCII characters,
# except for CR and LF.

# However, a field body may contain CRLF when
# used in header "folding" and "unfolding" as described in section
# 2.2.3.

method :read_headers(Str $head) {
  my @head_order;
  my ($curhead, $head_hash) = ("", {});

  for split /$crlf/, $head {
    if s/^\s+// or not /^([^:]+):\s*(.*)/ {
      next if !$curhead; # Well, that sucks.
      # This is a continuation line. We fold it onto the end of
      # the previous header.
      chomp $head_hash{$curhead}[-1];
      $head_hash{$curhead}[-1] ~= $head_hash{$curhead}[-1] ? " $_" : $_;
    } else {
      $curhead = $1;
      push $head_hash{$curhead}, $2;
      push @head_order, $curhead;
    }
  }

  return $head_hash, \@head_order;
}

=head2 header

Returns or sets a list of the contents of the given header.

=cut

# XXX: Implement that proxy thing...
#sub header(Str $field) is rw { %.header_names{lc $field} }
#
#sub header_set {
#    my ($self, $field, @data) = @_;
#    if ($GROUCHY) {
#        croak "I am not going to break RFC2822 and neither are you"
#            unless $field =~ /^[\x21-\x39\x3b-\x7e]+$/;
#        carp "You're a miserable bastard but I'll let you off this time"
#            unless $field =~ /^[\w-]+$/;
#    }
#
#    if (!exists $self->{header_names}->{lc $field}) {
#        $self->{header_names}->{lc $field} = $field;
#        # New fields are added to the end.
#        push @{$self->{order}}, $field;
#    } else {
#        $field = $self->{header_names}->{lc $field};
#    }
#
#    $self->{head}->{$field} = [ @data ];
#    return wantarray ? @data : $data[0];
#}

=head2 body

Returns the body text of the mail.

=cut

=head2 as_string

Returns the mail as a string, reconstructing the headers. Please note
that header fields are kept in order if they are unique, but, for,
instance, multiple "Received" headers will be grouped together. (This is
in accordance with RFC2822, honest.)

Also, if you've added new headers with C<header_set> that weren't in the
original mail, they'll be added to the end.

=cut

# However, for the purposes of this standard, header
# fields SHOULD NOT be reordered when a message is transported or
# transformed.  More importantly, the trace header fields and resent
# header fields MUST NOT be reordered, and SHOULD be kept in blocks
# prepended to the message.

method as_string { .:headers_as_string ~ $:mycrlf ~ $.body }

method :headers_as_string {
  my @order = $.order;
  my %head  = $:head_hash;
  my $stuff = "";

  while keys %head {
    my $thing = shift @order;
    next unless exists %head{$thing}; # We have already dealt with it
    $stuff ~= .:header_as_string($thing, %head{$thing});
    delete %head{$thing};
  }

  return $stuff;
}

method :header_as_string(Str $field, Array $data) {
  my @stuff = @$data;
  # Ignore "empty" headers
  return '' unless @stuff = grep:{ defined $_ } @stuff;
  return join "", map { $^a = "$field: $^a$:mycrlf";
			length > 78 ? .:fold($^a) : $^a }
		  @stuff;
}

method :fold(Str $line is copy) {
  # We know it will not contain any new lines at present
  my $folded = "";

  while $line {
    $line ~~ s/^\s+//;
    if $line ~~ s/^(.**{0..77})(\s|$)//) {
      $folded ~= $1 ~ $:mycrlf;
      $folded ~= " " if $line;
    } else {
      # Basically nothing we can do. :(
      $folded ~= $line;
      last;
    }
  }

  return $folded;
}

1;

__END__

=head1 CAVEATS

Email::Simple handles only RFC2822 formatted messages.  This means you
cannot expect it to cope well as the only parser between you and the
outside world, say for example when writing a mail filter for
invocation from a .forward file (for this we recommend you use
L<Email::Filter> anyway).  For more information on this issue please
consult RT issue 2478, http://rt.cpan.org/NoAuth/Bug.html?id=2478 .

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Ingo Blechschmidt (port to Perl 6)

Copyright 2004 by Casey West

Copyright 2003 by Simon Cozens

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

Perl Email Project, L<http://pep.kwiki.org>.

=cut
