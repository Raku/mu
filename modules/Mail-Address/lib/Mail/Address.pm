module Mail::Address-1.66;
use v6;

# Copyright (c) 1995-2001 Graham Barr <gbarr@pobox.com>.  All rights reserved.
# Copyright (c) 2002-2003 Mark Overmeer <mailtools@overmeer.net>
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

class Mail::Address {
  has Str ($.phrase, $.address, $.comment) is rw;

  submethod BUILD ($.phrase, $.address, ?$.comment) {}

  #
  # given a comment, attempt to extract a person's name
  #
  method :extract_name(Str $comment) {
    given $comment {
      # trim whitespace
      s/^\s+//;
      s/\s+$//;
      s/\s+/ /;

      # Disregard numeric names (e.g. 123456.1234@compuserve.com)
      return if /^<[\d ]>+$/;

      # remove outermost parenthesis
      s/^\((.*)\)$/$1/;

      # remove outer quotation marks
      s/^"(.*)"$/$1/;

      # remove minimal embedded comments
      s:g/\(.*?\)//;

      # remove all escapes
      s:g/\\//g;

      # remove internal quotation marks
      s/^"(.*)"$/$1/;

      # reverse "Last, First M." if applicable
      s/^(<-<space>>+) ?, ?(.*)$/$2 $1/;
      s/,.*//;

      # Change casing only when the name contains only upper or only
      # lower cased characters.
      unless m/[A-Z]/ and m/[a-z]/ {
	# Set the case of the name to first char upper rest lower
	# Upcase first letter on name
	s:ig/\b(\w+)/{ucfirst lc $1}/;

	# Scottish names such as 'McLeod'
	s:ig/\bMc(\w)/Mc{ucfirst $1}/;

	# Irish names such as 'O'Malley, O'Reilly'
	s:ig/\bo'(\w)/O'{ucfirst $1}/;

	# Roman numerals, eg 'Level III Support'
	s:ig/\b(x*(ix)?v*(iv)?i*)\b/{uc $1}/; 
      }

      # some cleanup
      s:g/\[<-[\]]>*\]//;
      s:g/(^<+<space>+['"]>+|<+<space>+['"]>+$)//;
      s:g/<space>**{2..}/ /;
      # "/ # -- ncecessary for ViM perl5 syntax highlighting.
    }

    return $_;
  }

  method :tokenise(Str $line) {
    given $line {
      my (@words, $snippet, $field);
 
      s/^\s+//;
      s:g/<[\r\n]>+/ /; # / # -- again, p5 vim syntax highlighting

      while $_ ne '' {
	$field = '';
	if s/^\s*\(/(/ {
	  my $depth = 0;

	  PAREN: while s/^(\((<-[\(\)\\]>|\\.)*)// {
	    $field ~= $1;
	    $depth++;
	    while s/^((<-[\(\)\\]>|\\.)*\)\s*)// {
	      $field ~= $1;
	      last PAREN unless --$depth;
	      $field ~= $1 if s/^((<-[\(\)\\]>|\\.)+)//;
	    }
	  }

	  die "Unmatched () '$field' '$_'" if $depth;

          $field ~~ s/\s+$//;
          push @words, $field;

	  next;
	}

	s/^("(<-["\\]>|\\.)*")\s*//               || # "..."
	s/^(\[(<-[\]\\]>|\\.)*\])\s*//            || # [...]
	s/^(<+<space>-[()<>\@,;:\\".\[\]]>+)\s*// || 
	s/^(<[()<>\@,;:\\".\[\]]>+)\s*//
	and do { push @words, $1; next };

	die "Unrecognised line: $_";
      }

      push(@words, ",");
    }

   return @words;
  }

  method :find_next(Num $idx is copy, Num $len is copy, Str *@tokens) {
    while $idx < $len {
      my $c = @tokens[$idx];
      return $c if $c eq ','|';''<';
      $idx++;
    }

    return;
  }

  method :complete(Str @phrase, Str @address, Str @comment) {
    return .new(
      phrase  => @phrase.join(" "),
      address => @address.join(""),
      comment => @address.join(" "),
    ) if @phrase or @comment or @address;
    return undef;
  }

  method parse(Str @line) {
    @line   .= grep { defined $_ };
    my $line = @line.join("");

    my @phrase  = ();
    my @comment = ();
    my @address = ();
    my @objs    = ();
    my $depth   = 0;
    my $idx     = 0;
    my @tokens  = .:tokenise($line);
    my $len     = +@tokens;
    my $next    = .:find_next($idx, $len, *@tokens);

    loop(; $idx < $len; $idx++) {
      given @tokens[$idx] {
	if substr($_,0,1) eq "(" {
	  push @comment, $_;
	} elsif $_ eq '<' {
	  $depth++;
	} elsif $_ eq '>' {
	  $depth-- if $depth;
	} elsif $_ eq ','|';' {
	  warn "Unmatched '<>' in \"$line\"" if $depth;
	  my $o = .:complete(phrase => @phrase, address => @address, comment => @comment);
	  if $o {
	    push @objs, $o;
	    (@phrase, @address, @comment) = ();
	  }
	  $depth = 0;
	  $next = .:find_next($idx+1, $len, *@tokens);
	} elsif $depth {
	  push @address, $_;
	} elsif $next eq "<" {
	  push @phrase, $_;
	} elsif m/^<[.\@:;]>$/ or !@address or @address[-1] ~~ m/^<[.\@:;]>$/ {
	  push @address, $_;
	} else {
	  warn "Unmatched '<>' in \"$line\"" if $depth;
	  my $o = .:complete(phrase => @phrase, address => @address, comment => @comment);
	  if $o {
	    push @objs, $o;
	    (@phrase, @address, @comment) = ();
	  }
	  $depth = 0;
	  push @address, $_;
	}
      }
    }

    return @objs;
  }

  method format() {
    my @fmts  = ();

    my $atext = rx/<+<space>+[\- !#$%&\'*+/=?^`{|}~]>/; #

    my ($phrase, $addr, $comment) = ($.phrase, $.addr, $.comment);
    my @tmp = ();

    if defined $phrase && length $phrase {
      push @tmp,
	$phrase ~~ m/^[\s*$atext\s*]+$/ ?? $phrase ::
	$phrase =~ /(<after !\\)"/      ?? $phrase :: "\"$phrase\"";

      push @tmp, "<$addr>" if defined $addr && length $addr;
    } else {
      push @tmp, $addr if defined $addr && length $addr;
    }

    if defined $comment && $comment ~~ m/\S/) {
      $comment ~~ s/^\s*\(?/(/;
      $comment ~~ s/\)?\s*$/)/;
    }

    push @tmp, $comment if defined $comment && length $comment;
    push @fmts, @tmp.join(" ") if @tmp;

    return @fmts.join(", ");
  }

  method name() {
    my ($phrase, $addr) = ($.phrase. $.address);
    
    $phrase  = $.comment unless defined $phrase && length $phrase;
    my $name = .:extract_name($phrase);

    # first.last@domain address
    if $name eq '' && $addr ~~ m/(<-[\%\.\@_]>+(<[\._]><-[\%\.\@_]>+)+)<[\@\%]>/ {
      ($name = $1) =~ s:g/<[\._]>+/ /; # / # -- vim perl6 syntax highlighting fix
      $name = .:extract_name($name);
    }

    if $name eq '' && $addr ~~ m#/g=#) {
      #X400 style address
      my ($f) = $addr ~~ m:i#g=(<-[/]>*)#;
      my ($l) = $addr ~~ m:i#s=(<-[/]>*)#;
	
      $name = .:extract_name("$f $l");
    }
       
    return length $name ? $name : undef;
  }


  method host() {
    my $addr = $.address err "";
    my $i    = rindex $addr, '@';

    my $host = ($i >= 0) ?? substr $addr, $i+1 : undef;

    return $host;
  }

  method user() {
    my $addr = $.address;
    my $i    = index $addr, '@';

    my $user = ($i >= 0) ?? substr $addr, 0, $i :: $addr;

    return $user;
  }

  method path()  {...}
  method canon() {( .host, .user, .path )}
}

1;


__END__

=head1 NAME

Mail::Address - Parse mail addresses

=head1 SYNOPSIS

    use Mail::Address;
    
    my @addrs = Mail::Address.parse($line);
    
    for @addrs -> $addr {
      print "{$addr.format}\n";
    }

=head1 DESCRIPTION

C<Mail::Address> extracts and manipulates email addresses from a message
header.  It cannot be used to extract addresses from some random text.
You can use this module to create RFC822 compliant fields.

Although C<Mail::Address> is a very popular subject for books, and is
used in many applications, it does a very poor job on the more complex
message fields.  It does only handle simple address formats (which
covers about 95% of what can be found). Problems are with

=over 4

=item *

no support for address groups, even not with the semi-colon as
separator between addresses

=item *

Limitted support for escapes in phrases and comments.  There are
cases where it can get wrong.

=item *

You have to take care of most escaping when you create an address yourself:
C<Mail::Address> does not do that for you.

=back

Often requests are made to improve this situation, but this is not a
good idea, where it will break zillions of existing applications.  If
you wish for a fully RFC2822 compliant implementation you may take a look
at L<Mail::Message::Field::Full>, part of MailBox.

=head1 CONSTRUCTORS

=over 4

=item new($phrase, $address, ?$comment)

  Mail::Address.new(
    phrase  => "Perl5 Porters",
    address => 'perl5-porters@africa.nicoh.com',
  );

Create a new C<Mail::Address> object which represents an address with the
elements given. In a message these 3 elements would be seen like:

 PHRASE <ADDRESS> (COMMENT)
 ADDRESS (COMMENT)

=item parse( LINE )

 Mail::Address.parse($line);

Parse the given line a return a list of extracted C<Mail::Address> objects.
The line would normally be one taken from a To,Cc or Bcc line in a message

=back

=head1 METHODS

=over 4

=item phrase ()

Return the phrase part of the object.

=item address ()

Return the address part of the object.

=item comment ()

Return the comment part of the object

=item format ()

Return a string representing the address in a suitable form to be placed
on a To,Cc or Bcc line of a message

=item name ()

Using the information contained within the object attempt to identify what
the person or groups name is

=item host ()

Return the address excluding the user id and '@'

=item user ()

Return the address excluding the '@' and the mail domain

=item path ()

Unimplemented yet but should return the UUCP path for the message

=item canon ()

Unimplemented yet but should return the UUCP canon for the message

=back

=head1 AUTHOR

Port to Perl 6: Ingo Blechschmidt

Graham Barr.  Maintained by Mark Overmeer <mailtools@overmeer.net>

=head1 COPYRIGHT

Copyright (c) 2002-2003 Mark Overmeer, 1995-2001 Graham Barr. All rights
reserved. This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
