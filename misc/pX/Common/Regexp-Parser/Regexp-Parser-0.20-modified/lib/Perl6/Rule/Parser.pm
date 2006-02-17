package Perl6::Rule::Parser;

use base 'Regexp::Parser';


our $non_nest_brack = qr{
  \[ [^]]* \] |
   { [^}]*  } |
   < [^>]*  > |
  \( [^)]* \)
}x;


sub nextchar {
  my ($self) = @_;

  ${&Rx} =~ m{ \G (?: \s+ | \# .* )+ }xgc;
}


sub match_scalar { }


sub match_array { }


sub match_hash { }


sub init {
  my ($self) = @_;

  $self->add_handler('atom' => sub {
    my ($S) = @_;
    $S->nextchar;

    ${&Rx} =~ m{ \G (.) }xgcs or return;
    my $c = $1;

    push @{ $S->{next} }, qw< atom >;
    return $S->$c if $S->can($c);
    return $S->object(exact => $c);
  });

  ### BACKSLASHED THINGS

  $self->add_handler('\\' => sub {
    my ($S, $cc) = @_;
    my $c = '\\';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      $c .= (my $n = $1);

      return $S->$c($cc) if $S->can($c);
      --&RxPOS;

      $S->warn(RPe_BADESC, $c = $n, "") if $n =~ /[a-zA-Z]/;

      return $S->object(exact => $n, $c);
    }

    $S->error(RPe_ESLASH);
  });

  $self->add_handler('\b' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\b", '\b') if $cc;
    return $S->object(bound => bound => '\b');
  });

  $self->add_handler('\B' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "B", " in character class") if $cc;
    return $S->force_object(anyof_char => 'B') if $cc;
    return $S->object(bound => nbound => '\B');
  });

  $self->add_handler('\c' => sub {
    my ($S, $cc) = @_;

    if (${&Rx} =~ m{ \G ($non_nest_brack) }xgc) {
      my $n = substr($1, 1, -1);
      my @names = split /;/, $n;

      if ($cc) {
        $S->error(0, "\\c[A;B] in character class") if @names > 1;
        return $S->force_object(anyof_char => $S->nchar($n), "\\c[$n]");
      }

      return $S->object(exact => join("", $S->nchar(@names)), "\\c[$n]");
    }

    $S->error(0, "Missing right %s on \\%s", 'brace', "c$1") if ${&Rx} =~ m{ \G ([[({<]) }xgc;
    $S->error(0, "Missing brackets on \\%s", 'c');
  });

  $self->add_handler('\C' => sub {
    my ($S, $cc) = @_;

    if (${&Rx} =~ m{ \G ($non_nest_brack) }xgc) {
      my $n = substr($1, 1, -1);
      my @names = split /;/, $n;

      if ($cc) {
        $S->error(0, "\\C[A;B] in character class") if @names > 1;
        return $S->force_object(anyof_char_comp => $S->nchar($n), "\\C[$n]");
      }

      return $S->object(exact_comp => join("", $S->nchar(@names)), "\\C[$n]");
    }

    $S->error(0, "Missing right %s on \\%s", 'brace', "C$1") if ${&Rx} =~ m{ \G ([[({<]) }xgc;
    $S->error(0, "Missing brackets on \\%s", 'C');
  });

  $self->add_handler('\d' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(digit => 0)) if $cc;
    return $S->object(digit => 0);
  });

  $self->add_handler('\D' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(digit => 1)) if $cc;
    return $S->object(digit => 1);
  });

  $self->add_handler('\e' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\e", '\e') if $cc;
    return $S->object(exact => "\e", '\e');
  });

  $self->add_handler('\E' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char_comp => "\e", '\E') if $cc;
    return $S->object(exact_comp => "\e", '\E');
  });

  $self->add_handler('\f' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\f", '\f') if $cc;
    return $S->object(exact => "\f", '\f');
  });

  $self->add_handler('\F' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char_comp => "\f", '\F') if $cc;
    return $S->object(exact_comp => "\f", '\F');
  });

  $self->add_handler('\h' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(horiz => 0)) if $cc;
    return $S->object(horiz => 0);
  });

  $self->add_handler('\H' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(horiz => 1)) if $cc;
    return $S->object(horiz => 1);
  });

  $self->add_handler('\n' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\n", '\n') if $cc;
    return $S->object(exact => "\n", '\n');
  });

  $self->add_handler('\N' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char_comp => "\n", '\N') if $cc;
    return $S->object(exact_comp => "\n", '\N');
  });

  $self->add_handler('\r' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\r", '\r') if $cc;
    return $S->object(exact => "\r", '\r');
  });

  $self->add_handler('\R' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char_comp => "\r", '\R') if $cc;
    return $S->object(exact_comp => "\r", '\R');
  });

  $self->add_handler('\s' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(space => 0)) if $cc;
    return $S->object(space => 0);
  });

  $self->add_handler('\S' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(space => 1)) if $cc;
    return $S->object(space => 1);
  });

  $self->add_handler('\t' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\t", '\t') if $cc;
    return $S->object(exact => "\t", '\t');
  });

  $self->add_handler('\T' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char_comp => "\t", '\T') if $cc;
    return $S->object(exact_comp => "\t", '\T');
  });

  $self->add_handler('\v' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(vert => 0)) if $cc;
    return $S->object(vert => 0);
  });

  $self->add_handler('\V' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(vert => 1)) if $cc;
    return $S->object(vert => 1);
  });

  $self->add_handler('\w' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(alnum => 0)) if $cc;
    return $S->object(alnum => 0);
  });

  $self->add_handler('\W' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(alnum => 1)) if $cc;
    return $S->object(alnum => 1);
  });

  $self->add_handler('\x' => sub {
    my ($S, $cc) = @_;

    if (${&Rx} =~ m{ \G ($non_nest_brack) }xgc) {
      my $h = substr($1, 1, -1);
      my @hex = split /;/, $h;

      $S->warn(0, "Illegal hexadecimal digit '%s' ignored", $1) if $h =~ /([^a-fA-F0-9;])/;

      if ($cc) {
        $S->error(0, "\\x[A;B] in character class") if @hex > 1;
        return $S->force_object(anyof_char => chr(hex $h), "\\x[$h]");
      }

      return $S->object(exact => join("", map chr(hex), @hex), "\\x[$h]");
    }

    $S->error(0, "Missing right %s on \\%s", 'brace', "x$1") if ${&Rx} =~ m{ \G ([[({<]) }xgc;

    if (${&Rx} =~ m{ \G ( [a-fA-F0-9]+ ) }xgc) {
      return $S->force_object(anyof_char => chr(hex $1), "\\x$1") if $cc;
      return $S->object(exact => chr(hex $1), "\\x$1");
    }

    $S->warn(0, "Illegal hexadecimal digit '%s' ignored", substr(${&Rx}, &RxPOS, 1));
    return $S->force_object(anyof_char => "\0", "\\x[0]") if $cc;
    return $S->object(exact => "\0", "\\x[0]");
  }

  $self->add_handler('\X' => sub {
    my ($S, $cc) = @_;

    if (${&Rx} =~ m{ \G ($non_nest_brack) }xgc) {
      my $h = substr($1, 1, -1);
      my @hex = split /;/, $h;

      $S->warn(0, "Illegal hexadecimal digit '%s' ignored", $1) if $h =~ /([^a-fA-F0-9;])/;

      if ($cc) {
        $S->error(0, "\\X[A;B] in character class") if @hex > 1;
        return $S->force_object(anyof_char_comp => chr(hex $h), "\\X[$h]");
      }

      return $S->object(exact_comp => join("", map chr(hex), @hex), "\\X[$h]");
    }

    $S->error(0, "Missing right %s on \\%s", 'brace', "X$1") if ${&Rx} =~ m{ \G ([[({<]) }xgc;

    if (${&Rx} =~ m{ \G ( [a-fA-F0-9]+ ) }xgc) {
      return $S->force_object(anyof_char_comp => chr(hex $1), "\\X$1") if $cc;
      return $S->object(exact_comp => chr(hex $1), "\\X$1");
    }

    $S->warn(0, "Illegal hexadecimal digit '%s' ignored", substr(${&Rx}, &RxPOS, 1));
    return $S->force_object(anyof_char_comp => "\0", "\\X[0]") if $cc;
    return $S->object(exact_comp => "\0", "\\X[0]");
  }

  $self->add_handler('\0' => sub {
    my ($S, $cc) = @_;

    if (${&Rx} =~ m{ \G ($non_nest_brack) }xgc) {
      my $o = substr($1, 1, -1);
      my @oct = split /;/, $o;

      $S->warn(0, "Illegal octal digit '%s' ignored", $1) if $o =~ /([^0-7;])/;

      if ($cc) {
        $S->error(0, "\\0[A;B] in character class") if @oct > 1;
        return $S->force_object(anyof_char => chr(oct $o), "\\0[$o]");
      }

      return $S->object(exact => join("", map chr(oct), @oct), "\\0[$o]");
    }

    $S->error(0, "Missing right %s on \\%s", 'brace', "0$1") if ${&Rx} =~ m{ \G ([[({<]) }xgc;

    if (${&Rx} =~ m{ \G ( [0-7]+ ) }xgc) {
      return $S->force_object(anyof_char => chr(oct $1), "\\0$1") if $cc;
      return $S->object(exact => chr(oct $1), "\\0$1");
    }

    $S->warn(0, "Illegal octal digit '%s' ignored", substr(${&Rx}, &RxPOS, 1));
    return $S->force_object(anyof_char => "\0", "\\0[0]") if $cc;
    return $S->object(exact => "\0", "\\0[0]");
  });


  ### ':'

  $self->add_flag('i' =>  sub { 0x01 });

  $self->add_handler(':' => sub {
    my ($S) = @_;

    if (${&Rx} =~ m{ \G \: }xgc) {
      my $n = '::';
      return $S->$n;
    }

    if (${&Rx} =~ m{ \G ([a-z]+) }xgc) {
    }

  });

  $self->add_handler('::' => sub {
    my ($S) = @_;

    if (${&Rx} =~ m{ \G \: }xgc) {
      my $n = ':::';
      return $S->$n;
    }
  });

  $self->add_handler(':::' => sub {
    my ($S) = @_;
  });


  ### '#'

  # I don't think I need to handle this because the
  # nextchar() method skips whitespace and comments


  ### '$'

  $self->add_handler('$' => sub {
    my ($S) = @_;

    if (${&Rx} =~ m{ \G \$ }xgc) {
      my $n = '$$';
      return $S->$n;
    }

    if ($S->match_scalar) {
    }

    return $S->object(eol => eos => '$');
  });

  $self->add_handler('$$' => sub {
    my ($S) = @_;

    if ($S->match_scalar) {
    }

    return $S->object(eol => eol => '$$');
  });


  ### '@'


  ### '%'


  ### '^'

  $self->add_handler('^' => sub {
    my ($S) = @_;

    if (${&Rx} =~ m{ \G \^ }xgc) {
      my $n = '^^';
      return $S->$n;
    }

    return $S->object(bol => bos => '^');
  });

  $self->add_handler('^^' => sub {
    my ($S) = @_;
    return $S->object(bol => bol => '^^');
  });


  ### '&'


  ### '*'

  $self->add_handler('*' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 0, '');
  });


  ### '+'

  $self->add_handler('+' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 1, '');
  });


  ### '?'

  $self->add_handler('?' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 0, 1);
  });


  ### '('

  $self->add_handler('(' => sub {
    my ($S) = @_;
    $S->nextchar;

    push @{ $S->{next} }, qw< c) atom >;
    &SIZE_ONLY ? ++$S->{maxpar} : ++$S->{nparen};
    push @{ $S->{flags} }, &Rf;
    return $S->object(open => $S->{nparen});
  });


  ### ')'


  ### '{'


  ### '}'


  ### '['


  ### ']'


  ### '<'


  ### '>'


  ### '.'

  $self->add_handler('.' => sub {
    my ($S) = @_;
    return $S->object(reg_any => sany => '.');
  });


  ### '|'

  $self->add_handler('|' => sub {
    my ($S) = @_;
    return $S->object(branch =>);
  });


  return;

  # control character
  $self->add_handler('\c' => sub {
    my ($S, $cc) = @_;
    ${&Rx} =~ m{ \G (.?) }xgc;
    my $c = $1;
    return $S->force_object(anyof_char => chr(64 ^ ord $c), "\\c$c") if $cc;
    return $S->object(exact => chr(64 ^ ord $c), "\\c$c");
  });

  # nprop (not a unicode property)
  $self->add_handler('\P' => sub {
    my ($S, $cc) = @_;
    $S->error(RPe_EMPTYB, 'P') if ${&Rx} !~ m{ \G (.) }xgcs;

    my $name = $1;
    if ($name eq '{') {
      $S->error(RPe_RBRACE, 'P') if ${&Rx} !~ m{ \G ([^\}]*) \} }xgc;
      $name = $1;
    }

    return $S->force_object(anyof_class => $S->force_object(prop => $name, 1)) if $cc;
    return $S->object(prop => $name, 1);
  });

  # prop (a unicode property)
  $self->add_handler('\p' => sub {
    my ($S, $cc) = @_;
    $S->error(RPe_EMPTYB, 'p') if ${&Rx} !~ m{ \G (.) }xgcs;

    my $name = $1;
    if ($name eq '{') {
      $S->error(RPe_RBRACE, 'p') if ${&Rx} !~ m{ \G ([^\}]*) \} }xgc;
      $name = $1;
    }

    return $S->force_object(anyof_class => $S->force_object(prop => $name, 0)) if $cc;
    return $S->object(prop => $name, 0);
  });

  # clump (a unicode clump)
  $self->add_handler('\X' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, 'X', ' in character class') if $cc;
    return $S->force_object(anyof_char => 'X') if $cc;
    return $S->object(clump => '\X');
  });

  # hex character
  $self->add_handler('\x' => sub {
    my ($S, $cc) = @_;
    ${&Rx} =~ m{ \G ( \{ | .{0,2} ) }sxgc;
    my $brace = 0;
    my $num = $1;

    if ($num eq '{') {
      $S->error(RPe_RBRACE, 'x') if ${&Rx} !~ m{ \G ( [^\}]* ) \} }xgc;
      $num = $1;
      $brace = 1;
    }
    else {
      my $good = ($num =~ s/^([a-fA-F0-9]*)// and $1);
      &RxPOS -= length $num;
      $num = $good;
    }

    my $rep = $brace ? "\\x{$num}" : sprintf("\\x%02s", $num);
    return $S->force_object(anyof_char => chr hex $num, $rep) if $cc;
    return $S->object(exact => chr hex $num, $rep);
  });

  # alpha POSIX class
  $self->add_handler('POSIX_alpha' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => alpha => $neg, \$how);
  });

  # alnum POSIX class
  $self->add_handler('POSIX_alnum' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => alnum => $neg, \$how);
  });

  # ascii POSIX class
  $self->add_handler('POSIX_ascii' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => ascii => $neg, \$how);
  });

  # cntrl POSIX class
  $self->add_handler('POSIX_cntrl' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => cntrl => $neg, \$how);
  });

  # digit POSIX class
  $self->add_handler('POSIX_digit' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => digit => $neg, \$how);
  });

  # graph POSIX class
  $self->add_handler('POSIX_graph' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => graph => $neg, \$how);
  });

  # lower POSIX class
  $self->add_handler('POSIX_lower' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => lower => $neg, \$how);
  });

  # print POSIX class
  $self->add_handler('POSIX_print' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => print => $neg, \$how);
  });

  # punct POSIX class
  $self->add_handler('POSIX_punct' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => punct => $neg, \$how);
  });

  # space POSIX class
  $self->add_handler('POSIX_space' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => space => $neg, \$how);
  });

  # upper POSIX class
  $self->add_handler('POSIX_upper' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => upper => $neg, \$how);
  });

  # word POSIX class
  $self->add_handler('POSIX_word' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => word => $neg, \$how);
  });

  # xdigit POSIX class
  $self->add_handler('POSIX_xdigit' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => xdigit => $neg, \$how);
  });

  $self->add_handler('{' => sub {
    my ($S) = @_;
    if (${&Rx} =~ m{ \G (\d+) (,?) (\d*) \} }xgc) {
      my ($min, $range, $max) = ($1, $2, $3);
      $max = $min unless $range;
      push @{ $S->{next} }, qw< minmod >;
      $S->error(RPe_BCURLY) if length($max) and $min > $max;
      return $S->object(quant => $min, $max);
    }
    return $S->object(exact => '{');
  });

  $self->add_handler('minmod' => sub {
    my ($S) = @_;
    $S->nextchar;
    return $S->object(minmod =>) if ${&Rx} =~ m{ \G \? }xgc;
    return;
  });

  # alternation branch

  # opening parenthesis (maybe capturing paren)

  # any character

  # backslash

  # start of char class (and possible negation)
  $self->add_handler('[' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< cce] cc cc] >;
    my $neg = ${&Rx} =~ m{ \G \^ }xgc;

    my $pos = &RxPOS;
    if (${&Rx} =~ m{ \G ([:.=]) .*? \1 ] }xgc) {
      $S->warn(RPe_OUTPOS, $1, $1);
      &RxPOS = $pos;
    }

    return $S->object(anyof => $neg);
  });

  # char class ] at beginning
  $self->add_handler('cc]' => sub {
    my ($S) = @_;
    return unless ${&Rx} =~ m{ \G ] }xgc;
    return $S->object(anyof_char => "]");
  });

  # start of char class range (or maybe just char)
  $self->add_handler('cc' => sub {
    my ($S) = @_;
    return if ${&Rx} =~ m{ \G (?= ] | \z ) }xgc;
    push @{ $S->{next} }, qw< cc >;
    my ($lhs, $rhs, $before_range);
    my $ret = \$lhs;

    {
      if (${&Rx} =~ m{ \G ( \\ ) }xgcs) {
        my $c = $1;
        $$ret = $S->$c(1);
      }
      elsif (${&Rx} =~ m{ \G \[ ([.=:]) (\^?) (.*?) \1 \] }xgcs) {
        my ($how, $neg, $name) = ($1, $2, $3);
        my $posix = "POSIX_$name";
        if ($S->can($posix)) { $$ret = $S->$posix($neg, $how) }
        else { $S->error(RPe_BADPOS, "$how$neg$name$how") }
      }
      elsif (${&Rx} =~ m{ \G (.) }xgcs) {
        $$ret = $S->force_object(anyof_char => $1);
      }

      if ($ret == \$lhs) {
        if (${&Rx} =~ m{ \G (?= - ) }xgc) {
          if ($lhs->visual =~ /^(?:\[[:.=]|\\[dDsSwWpP])/) {
            $S->warn(RPe_FRANGE, $lhs->visual, "");
            $ret = $lhs;
            last;
          }
          $before_range = &RxPOS++;
          $ret = \$rhs;
          redo;
        }
        $ret = $lhs;
      }
      elsif ($ret == \$rhs) {
        if ($rhs->visual =~ /^(?:\[[:.=]|\\[dDsSwWpP])/) {
          $S->warn(RPe_FRANGE, $lhs->visual, $rhs->visual);
          &RxPOS = $before_range;
          $ret = $lhs;
        }
        elsif ($lhs->visual gt $rhs->visual) {
          $S->error(RPe_IRANGE, $lhs->visual, $rhs->visual);
        }
        else {
          $ret = $S->object(anyof_range => $lhs, $rhs);
        }
      }
    }

    return if &SIZE_ONLY;
    return $ret;
  });

  # end of char class
  $self->add_handler('cce]' => sub {
    my ($S) = @_;
    $S->error(RPe_LBRACK) if ${&Rx} !~ m{ \G ] }xgc;
    return $S->object(anyof_close => "]");
  });

  # closing paren coming from 'atom'
  $self->add_handler(')' => sub {
    my ($S) = @_;
    pop @{ $S->{next} };
    &RxPOS--;
    return;
  });

  # closing paren coming from an opening paren
  $self->add_handler('c)' => sub {
    my ($S) = @_;
    $S->error(RPe_LPAREN) if ${&Rx} !~ m{ \G \) }xgc;
    pop @{ $S->{flags} };
    return $S->object(close =>);
  });

  # some kind of assertion...
  $self->add_handler('(?' => sub {
    my ($S) = @_;
    my $c = '(?';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
      &RxPOS--;
    }
    else {
      $S->error(RPe_SEQINC);
    }

    # flag assertion or non-capturing group
    ${&Rx} =~ m{ \G ([a-zA-Z]*) (-? [a-zA-Z]*) }xgc;
    my ($on, $off) = ($1, $2);
    my ($r_on, $r_off) = ("", "");
    my ($f_on, $f_off) = (0,0);

    &RxPOS -= length($on.$off);
    my $old = &RxPOS;

    for (split //, $on) {
      &RxPOS++;
      if (my $f = $S->can("FLAG_$_")) {
        my $v = $S->$f(1) and $r_on .= $_;
        $f_on |= $v;
        next;
      }
      my $bad = substr ${&Rx}, $old;
      $S->error(RPe_NOTREC, &RxPOS - $old, $bad);
    }

    &RxPOS++ if $off =~ s/^-//;

    for (split //, $off) {
      &RxPOS++;
      if (my $f = $S->can("FLAG_$_")) {
        my $v = $S->$f(0) and $r_off .= $_;
        $f_off |= $v;
        next;
      }
      my $bad = substr ${&Rx}, $old;
      $S->error(RPe_NOTREC, &RxPOS - $old, $bad);
    }

    if (${&Rx} =~ m{ \G ([:)]) }xgc) {
      my $type = $1 eq ':' ? 'group' : 'flags';
      if ($type eq 'group') {
        push @{ $S->{flags} }, &Rf;
        push @{ $S->{next} }, qw< c) atom >;
      }
      &Rf |= $f_on;
      &Rf &= ~$f_off;
      return $S->object($type => $r_on, $r_off);
    }

    &RxPOS++;
    my $l = length($on.$off) + 2;
    $S->error(RPe_NOTREC, $l, substr(${&Rx}, $old));
  });

  # comment
  $self->add_handler('(?#' => sub {
    my ($S) = @_;
    ${&Rx} =~ m{ \G [^)]* }xgc;
    $S->error(RPe_NOTERM) unless ${&Rx} =~ m{ \G \) }xgc;
    return;
  });

  # not implemented (?$...)
  $self->add_handler('(?$' => sub {
    my ($S) = @_;
    $S->error(RPe_NOTREC, 1, substr(${&Rx}, &RxPOS - 1));
  });

  # not implemented (?@...)
  $self->add_handler('(?@' => sub {
    my ($S) = @_;
    $S->error(RPe_NOTREC, 1, substr(${&Rx}, &RxPOS - 1));
  });

  # look-ahead
  $self->add_handler('(?=' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< c) atom >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(ifmatch => 1);
  });

  # look-ahead (neg)
  $self->add_handler('(?!' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< c) atom >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(unlessm => 1);
  });

  # look-behind prefix
  $self->add_handler('(?<' => sub {
    my ($S) = @_;
    my $c = '(?<';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
    }

    $S->error(RPe_NOTREC, 2, substr(${&Rx}, &RxPOS - 2));
  });

  # look-behind
  $self->add_handler('(?<=' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< c) atom >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(ifmatch => -1);
  });

  # look-behind (neg)
  $self->add_handler('(?<!' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< c) atom >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(unlessm => -1);
  });

  # suspend
  $self->add_handler('(?>' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< c) atom >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(suspend =>);
  });

  # eval
  $self->add_handler('(?{' => sub {
    my ($S) = @_;
    my $nest;
    $nest = qr[ (?> [^\\{}]+ | \\. | { (??{ $nest }) } )* ]x;
    if (${&Rx} =~ m{ \G ($nest) \} \) }xgc) {
      push @{ $S->{flags} }, &Rf;
      return $S->object(eval => $1);
    }
    $S->error(RPe_NOTBAL);
  });

  # logical prefix
  $self->add_handler('(??' => sub {
    my ($S) = @_;
    my $c = '(??';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
    }

    $S->error(RPe_NOTREC, 2, substr(${&Rx}, &RxPOS - 2));
  });

  # logical
  $self->add_handler('(??{' => sub {
    my ($S) = @_;
    my $nest;
    $nest = qr[ (?> [^\\{}]+ | \\. | { (??{ $nest }) } )* ]x;
    if (${&Rx} =~ m{ \G ($nest) \} \) }xgc) {
      push @{ $S->{flags} }, &Rf;
      return $S->object(logical => $1);
    }
    $S->error(RPe_NOTBAL);
  });

  # logical prefix
  $self->add_handler('(?p' => sub {
    my ($S) = @_;
    my $c = '(?p';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
    }

    $S->error(RPe_NOTREC, 2, substr(${&Rx}, &RxPOS - 2));
  });

  # logical
  $self->add_handler('(?p{' => sub {
    my ($S) = @_;
    $S->warn(RPe_LOGDEP);
    my $c = "(??{";
    return $S->$c;
  });

  $self->add_handler('(?(' => sub {
    my ($S) = @_;
    my $c = '(?(';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
      &RxPOS--;
    }

    push @{ $S->{next} }, qw< ifthen( >;
    push @{ $S->{flags} }, &Rf;
    return $S->object(ifthen =>);
  });

  # (?(...)t|f) condition
  $self->add_handler('ifthen(' => sub {
    my ($S) = @_;
    my $c = 'ifthen(';

    push @{ $S->{next} }, qw< c) atom >;

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
      &RxPOS--;
    }

    if (${&Rx} =~ m{ \G ( [1-9]\d* ) }xgc) {
      my $n = $1;
      $S->error(RPe_SWNREC) if ${&Rx} !~ m{ \G \) }xgc;
      push @{ $S->{next} }, qw< ifthen|2 ifthen| ifthen_atom >;
      return $S->object(groupp => $n);
    }

    $S->error(RPe_SWUNKN, &RxCUR);
  });

  # atom inside an ifthen
  $self->add_handler('ifthen_atom' => sub {
    my ($S) = @_;
    $S->nextchar;
    ${&Rx} =~ m{ \G ([^|]) }xgcs or return;
    my $c = $1;

    push @{ $S->{next} }, qw< ifthen_atom >;
    return $S->$c if $S->can($c);
    return $S->object(exact => $c);
  });

  # alternation branch inside ifthen
  $self->add_handler('ifthen|' => sub {
    my ($S) = @_;
    return if ${&Rx} !~ m{ \G \| }xgc;
    push @{ $S->{next} }, qw< ifthen_atom >;
    return $S->object(branch =>);
  });

  # illegal 2nd alternation branch inside ifthen
  $self->add_handler('ifthen|2' => sub {
    my ($S) = @_;
    return if ${&Rx} !~ m{ \G \| }xgc;
    $S->error(RPe_SWBRAN);
  });

  $self->add_handler('ifthen(?' => sub {
    my ($S) = @_;
    my $c = '(?';

    push @{ $S->{next} }, qw< ifthen|2 ifthen| ifthen_atom >;

    if (${&Rx} =~ m{ \G ( (?: <? [!=] | \{ ) ) }xgc) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
      &RxPOS -= length $1;
    }

    $S->error(RPe_SEQINC);
  });
}


1;

__END__
