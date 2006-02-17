package Regexp::Parser;

sub init {
  my ($self) = @_;

  # /m, /s, /i, /x flags
  $self->add_flag('m' => sub { 0x1 });
  $self->add_flag('s' => sub { 0x2 });
  $self->add_flag('i' => sub { 0x4 });
  $self->add_flag('x' => sub { 0x8 });

  # (useless) /g, /c, /o flags
  $self->add_flag('g' => sub {
    my ($S, $plus) = @_;
    $S->warn(RPe_BADFLG, $plus ? "" : "-", "g", $plus ? "" : "don't ", "g");
    return 0x0;
  });
  $self->add_flag('c' => sub {
    my ($S, $plus) = @_;
    $S->warn(RPe_BADFLG, $plus ? "" : "-", "c", $plus ? "" : "don't ", "gc");
    return 0x0;
  });
  $self->add_flag('o' => sub {
    my ($S, $plus) = @_;
    $S->warn(RPe_BADFLG, $plus ? "" : "-", "o", $plus ? "" : "don't ", "o");
    return 0x0;
  });

  $self->add_handler('\a' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\a", '\a') if $cc;
    return $S->object(exact => "\a", '\a');
  });

  $self->add_handler('\e' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\e", '\e') if $cc;
    return $S->object(exact => "\e", '\e');
  });

  $self->add_handler('\f' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\f", '\f') if $cc;
    return $S->object(exact => "\f", '\f');
  });

  $self->add_handler('\n' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\n", '\n') if $cc;
    return $S->object(exact => "\n", '\n');
  });

  $self->add_handler('\r' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\r", '\r') if $cc;
    return $S->object(exact => "\r", '\r');
  });

  $self->add_handler('\t' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\t", '\t') if $cc;
    return $S->object(exact => "\t", '\t');
  });

  # bol, mbol, sbol
  $self->add_handler('^' => sub {
    my ($S) = @_;
    my $type =
      &Rf & $S->FLAG_m ? 'mbol' :
      &Rf & $S->FLAG_s ? 'sbol' :
      'bol';
    return $S->object(bol => $type, '^');
  });

  # sbol (beginning of line in single-line mode)
  $self->add_handler('\A' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "A", " in character class") if $cc;
    return $S->force_object(anyof_char => 'A') if $cc;
    return $S->object(bol => 'sbol' => '\A');
  });

  # nbound (not a word boundary)
  $self->add_handler('\B' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "B", " in character class") if $cc;
    return $S->force_object(anyof_char => 'B') if $cc;
    return $S->object(bound => nbound => '\B');
  });

  # bound (not a word boundary)
  $self->add_handler('\b' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_char => "\b", '\b') if $cc;
    return $S->object(bound => bound => '\b');
  });

  # cany (any byte)
  $self->add_handler('\C' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "C", " in character class") if $cc;
    return $S->force_object(anyof_char => 'C') if $cc;
    return $S->object(reg_any => c_any => '\C');
  });

  # control character
  $self->add_handler('\c' => sub {
    my ($S, $cc) = @_;
    ${&Rx} =~ m{ \G (.?) }xgc;
    my $c = $1;
    return $S->force_object(anyof_char => chr(64 ^ ord $c), "\\c$c") if $cc;
    return $S->object(exact => chr(64 ^ ord $c), "\\c$c");
  });

  # ndigit (not a digit)
  $self->add_handler('\D' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(digit => 1)) if $cc;
    return $S->object(digit => 1);
  });

  # digit (a digit)
  $self->add_handler('\d' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(digit => 0)) if $cc;
    return $S->object(digit => 0);
  });

  # gpos (last global match end)
  $self->add_handler('\G' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "G", " in character class") if $cc;
    return $S->force_object(anyof_char => 'G') if $cc;
    return $S->object(gpos => gpos => '\G');
  });

  # named (named character)
  $self->add_handler('\N' => sub {
    my ($S, $cc) = @_;
    $S->error(RPe_BRACES, 'N') if ${&Rx} !~ m{ \G \{ }xgc;
    $S->error(RPe_RBRACE, 'N') if ${&Rx} !~ m{ \G ([^\}]*) \} }xgc;

    my $name = $1;
    return $S->force_object(anyof_char => $S->nchar($name), "\\N{$name}") if $cc;
    return $S->object(exact => $S->nchar($name), "\\N{$name}");
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

  # nspace (not a space)
  $self->add_handler('\S' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(space => 1)) if $cc;
    return $S->object(space => 1);
  });

  # space (a space)
  $self->add_handler('\s' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(space => 0)) if $cc;
    return $S->object(space => 0);
  });

  # nalnum (not a word character)
  $self->add_handler('\W' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(alnum => 1)) if $cc;
    return $S->object(alnum => 1);
  });

  # alnum (a word character)
  $self->add_handler('\w' => sub {
    my ($S, $cc) = @_;
    return $S->force_object(anyof_class => $S->force_object(alnum => 0)) if $cc;
    return $S->object(alnum => 0);
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

  # eol, seol, meol
  $self->add_handler('$' => sub {
    my ($S) = @_;
    my $type =
      &Rf & $S->FLAG_m ? 'meol' :
      &Rf & $S->FLAG_s ? 'seol' :
      'eol';
    return $S->object(eol => $type, '$');
  });

  # seol (end of line, in single-line mode)
  $self->add_handler('\Z' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "Z", " in character class") if $cc;
    return $S->force_object(anyof_char => 'Z') if $cc;
    return $S->object(eol => seol => '\Z');
  });

  # eos (end of string)
  $self->add_handler('\z' => sub {
    my ($S, $cc) = @_;
    $S->warn(RPe_BADESC, "z", " in character class") if $cc;
    return $S->force_object(anyof_char => 'z') if $cc;
    return $S->object(eol => eos => '\z');
  });

  # alpha POSIX class
  $self->add_handler('POSIX_alpha' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => alpha => $neg, $how);
  });

  # alnum POSIX class
  $self->add_handler('POSIX_alnum' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => alnum => $neg, $how);
  });

  # ascii POSIX class
  $self->add_handler('POSIX_ascii' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => ascii => $neg, $how);
  });

  # cntrl POSIX class
  $self->add_handler('POSIX_cntrl' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => cntrl => $neg, $how);
  });

  # digit POSIX class
  $self->add_handler('POSIX_digit' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => digit => $neg, $how);
  });

  # graph POSIX class
  $self->add_handler('POSIX_graph' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => graph => $neg, $how);
  });

  # lower POSIX class
  $self->add_handler('POSIX_lower' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => lower => $neg, $how);
  });

  # print POSIX class
  $self->add_handler('POSIX_print' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => print => $neg, $how);
  });

  # punct POSIX class
  $self->add_handler('POSIX_punct' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => punct => $neg, $how);
  });

  # space POSIX class
  $self->add_handler('POSIX_space' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => space => $neg, $how);
  });

  # upper POSIX class
  $self->add_handler('POSIX_upper' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => upper => $neg, $how);
  });

  # word POSIX class
  $self->add_handler('POSIX_word' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => word => $neg, $how);
  });

  # xdigit POSIX class
  $self->add_handler('POSIX_xdigit' => sub {
    my ($S, $neg, $how) = @_;
    return $S->force_object(anyof_class => xdigit => $neg, $how);
  });

  $self->add_handler('atom' => sub {
    my ($S) = @_;
    $S->nextchar;

    ${&Rx} =~ m{ \G (.) }xgcs or return;
    my $c = $1;

    push @{ $S->{next} }, qw< atom >;
    return $S->$c if $S->can($c);
    return $S->object(exact => $c);
  });

  $self->add_handler('*' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 0, '');
  });

  $self->add_handler('+' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 1, '');
  });

  $self->add_handler('?' => sub {
    my ($S) = @_;
    push @{ $S->{next} }, qw< minmod >;
    return $S->object(quant => 0, 1);
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
  $self->add_handler('|' => sub {
    my ($S) = @_;
    return $S->object(branch =>);
  });

  # opening parenthesis (maybe capturing paren)
  $self->add_handler('(' => sub {
    my ($S) = @_;
    my $c = '(';
    $S->nextchar;

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      my $n = "$c$1";
      return $S->$n if $S->can($n);
      &RxPOS--;
    }

    push @{ $S->{next} }, qw< c) atom >;
    &SIZE_ONLY ? ++$S->{maxpar} : ++$S->{nparen};
    push @{ $S->{flags} }, &Rf;
    return $S->object(open => $S->{nparen});
  });

  # any character
  $self->add_handler('.' => sub {
    my ($S) = @_;
    my $family =
      &Rf & $S->FLAG_s ? 'sany' :
      'reg_any';
    return $S->object(reg_any => $family, '.');
  });

  # backslash
  $self->add_handler('\\' => sub {
    my ($S, $cc) = @_;
    my $c = '\\';

    if (${&Rx} =~ m{ \G (.) }xgcs) {
      $c .= (my $n = $1);

      return $S->$c($cc) if $S->can($c);

      if ($n =~ /\d/) {
        --&RxPOS;
        my $v = "";

        # outside of char class, \nnn might be backref
        if (!&SIZE_ONLY and !$cc and $n != 0) {
          $v .= $1 while ${&Rx} =~ m{ \G (\d) }xgc;
          if ($v > 9 and $v > $S->{maxpar}) {
            &RxPOS -= length $v;
            $v = "";
          }
          elsif ($v > $S->{maxpar}) { $S->error(RPe_BGROUP) }
          else { return $S->object(ref => $v, "\\$v") }
        }

        $v .= $1 while ${&Rx} =~ m{ \G ([0-7]) }xgc;
        return $S->object(exact => chr oct $v, sprintf("\\%03s", $v));
      }

      $S->warn(RPe_BADESC, $c = $n, "") if $n =~ /[a-zA-Z]/;

      return $S->object(exact => $n, $c);
    }

    $S->error(RPe_ESLASH);
  });

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

=head1 NAME

Regexp::Parser::Handlers - handlers for Perl 5 regexes

=head1 DESCRIPTION

This module holds the init() method for the F<Regexp::Parser> class,
which installs all the handlers for standard Perl 5 regexes.  This
documentation contains a sub-classing tutorial.

=head1 SUB-CLASSING

I will present two example sub-classes, F<Regexp::NoCode>, and
F<Regexp::AndBranch>.

=head2 Parser Internals

The parser object is a hash reference with the following keys:

=over 4

=item regex

A reference to the original string representation of the regex.

=item len

The length of the original string representation of the regex.

=item tree

During the first pass, C<tree> is undef, which instructs the object()
method not to actually create any objects.  Afterwards, it is an array
reference of (node) objects.

=item stack

Initially an array reference, used to store the tree as a new scope is
entered and then exited.  The general concept is:

  if (into_scope) {
    push STACK, TREE;
    TREE = CURRENT->DATA;
  }
  if (outof_scope) {
    TREE = pop STACK;
  }

After the tree has been created, this key is deleted; this gives the
code a way to be sure compilation was successful.

=item maxpar

The highest number of parentheses.  It will end up being identical to
C<nparen>, but it is incremented during the initial pass, so that on
the second pass (the tree-building), it can distinguish back-references
from octal escapes.  (The source code to Perl's regex compiler does the
same thing.)

=item nparen

The number of OPENs (capturing groups) in the regex.

=item captures

An array reference to the 'open' nodes.

=item flags

An array reference of flag values.  When a scope is entered, the top
value is copied and pushed onto the stack.  When a scope is left, the
top value is popped and discarded.

It is important to do this copy-and-push before you do any flag-parsing,
if you're adding a handle that might parse flags, because you do not
want to accidentally affect the previous scope's flag values.  

Here is example code from the handler for C<(?ismx)> and C<(?ismx:...)>:

  # (?i:...) <-- the 'i' is only inside the (?:...)
  # (?i)     <-- the 'i' affects the rest of this scope

  # so if we're a (?:...), copy-and-push
  if ($type eq 'group') {
    push @{ $S->{flags} }, &Rf;
    push @{ $S->{next} }, qw< c) atom >;
  }

  for (split //, $on) {
    if (my $h = $S->can("FLAG_$_")) {
      my $v = $h->(1);       # 1 means this is 'on'
      if ($v) { &Rf |= $v }  # turn the flag on
      else { ... }           # the flag's value is 0
      next;
    }
    # throw an error if the flag isn't supported
  }

  for (map "FLAG_$_", split //, $off) {
    if (my $h = $S->can("FLAG_$_")) {
      my $v = $h->(0);        # 0 means this is 'off'
      if ($v) { &Rf &= ~$v }  # turn the flag off
      else { ... }            # the flag's value is 0
      next;
    }
    # throw an error if the flag isn't supported
  }

You'll probably not be adding handlers that have to parse flags, but if
you do, remember to follow this model correctly.

=item next

An array reference of what handles (or "rules") to try to match next.

=back

=head2 Devices and Standards

I made a few C-macro-style functions for easy access to the parser
object's most important attributes:

  # access to the regex
    # reference to the regex string
    sub Rx :lvalue          { $_[0]{regex} }

    # the position in the regex string
    sub RxPOS :lvalue       { pos ${&Rx} }

    # the regex string from the current position on
    sub RxCUR               { substr ${&Rx}, &RxPOS }

    # the length of the regex string
    sub RxLEN               { $_[0]{len} }

  # access to the flag stack
    # the top flag value
    sub Rf :lvalue          { $_[0]{flags}[-1] }

  # access to the tree
    # is this the first pass?
    sub SIZE_ONLY           { ! $_[0]{tree} }

    # the most recent addition to the tree
    sub LATEST :lvalue      { $_[0]{tree}[-1] }

You may find it helpful to copy these to your sub-class.  If you're
curious why the C<regex> value is a reference, and thus why I'm using
C<${&Rx}> everywhere, it's because an lvalued subroutine returning a
normal scalar doesn't work quite right with a regex that's supposed to
update its target's C<pos()>.  This method, where it returns a
I<reference> to a scalar, makes it work (!).

B<These functions can only work if called with ampersands, and only if
the parser object is the first value in C<@_>.>  I made sure of this in
my code; you should make sure in yours.

Matching against the regex is done in scalar context, globally, like so:

  if (${&Rx} =~ m{ \G pattern }xgc) {
    # it matched
  }

If the match fails, the C<pos()> value won't be reset (due to the C</c>
modifier).  Remember to use I<scalar> context.  If you need to access
capture groups, use the digit variables, but only if you're sure the
match succeeded.

=head2 Parser Methods

=over 4

=item my $obj = $parser->object(TYPE => ARGS...)

This creates a node of package C<TYPE> and sends the constructor whatever
other arguments are included.  This method takes care of building the
proper inheritance for the node; it uses %Regexp::Parser::loaded to keep
track of which object classes have been loaded already.

=item $parser->init()

This method installs all the flags and handlers.  F<Regexp::Parser> does
this automatically, but if you are sub-classing it, you'll probably want
to call it in your own module.

  package Regexp::AndBranch;
  use base 'Regexp::Parser';

  sub init {
    my $self = shift;

    # installs Regexp::Parser's handlers
    $self->SUPER::init();

    # now add your own...
    $self->add_handler('&' => ...);  # see below
  }

=item $parser->add_flag($flag, $code)

This method creates a method of the parser C<FLAG_$flag>, and sets it to
the code reference in $code.  Example:

  $parser->add_flag("u" => sub { 0x10 });

This makes 'u' a valid flag for your regex, and creates the method
C<FLAG_u>.  This doesn't mean you can use them on C<qr//>, but rather
that you can write C<(?u:...)> or C<(?u)>.  The values 0x01, 0x02, 0x04,
and 0x08 are used for C</m>, C</s>, C</i>, and C</x> in Perl's regexes.

The flag handler gets the parser object and a boolean as arguments.
The boolean is true if the flag is going to be turned on, and false if
it's going to be turned off.  For C<(?i-s)>, C<FLAG_i> would be called
with a true argument, and C<FLAG_s> would be called with a false one.

If the flag handler returns 0, the flag is removed from the resulting
object's visual flag set, so C<(?ig-o)> becomes C<(?i)>.

=item $parser->del_flag(@flags)

Deletes the handlers for the flags -- you need only pass the flag names,
without the "FLAG_" prefix.

=item $parser->add_handler($seq, $code)

This method creates a method of the parser named $seq, and set it to
the code reference in $code.  Example:

  # continuing from above...
  sub init {
    my $self = shift;
    $self->SUPER::init();

    $self->add_handler('&' => sub {
      # $S will be the Regexp::AndBranch object, $self
      my ($S) = @_;
      push @{ $S->{next} }, qw< atom >;
      return $S->object('and');
    });
  }

There is a specific scheme to how you must name your handlers.  If you
want to install a handler for '&&', you must first install a handler for
'&' that calls the handler for '&&' if it can consume an ampersand.
Handle names that have no "predecessor" (that is, a '&&' without a '&')
are pre-consumption: that is, they have not matched something yet.
Handle names that I<do> have a "predecessor" (that is, a '&&' with a
'&') are post-consumption: they have already matched what they are
named.

The handle 'atom' is pre-consumptive (because there is no 'ato' handle,
basically).  In order for the 'atom' handle to be executed, you must
explicitly add it to the queue (C<< $parser->{next} >>).

The handle '|' is post-consumptive.  It happens to be executed when
'atom' matches a '|'.  This means the handler for '|' does not need
to match it; it has already been consumed.

If you created a handle for '&&' without a predecessor, you would have
to add it explicity to the queue for it to ever be executed.  As such,
it would be pre-consumptive.

There is an interesting case of the right parenthesis ')'.  There cannot
be one without a matching left parenthesis '('; if there is an extra ')'
a fatal error is thrown.  However, the nature of 'atom' is to match a
character, see if there's a handler installed, and call it if there is.
I don't want atom to handle ')', so the handler is:

  $self->add_handler(')' => sub {
    my ($S) = @_;
    pop @{ $S->{next} };  # there was an 'atom' there
    &RxPOS--;             # this does pos(regex)--
    return;
  });

This handler un-consumes the ')' (via C<&RxPOS-->) and returns false, to
pretend it didn't actually match.  The real closing parenthesis handler
is:

  $self->add_handler('c)' => sub {
    my ($S) = @_;
    $S->error(RPe_LPAREN) if ${&Rx} !~ m{ \G \) }xgc;
    pop @{ $S->{flags} };
    return $S->object(close =>);
  });

The name is 'c)' which has no predecessor 'c', so that means it is
pre-consumptive, which is why it must match the right parenthesis
itself.  The handler throws an error if it can't match the ')', because
if the 'c)' handler gets called, it's expected to match!  It pops the
flag stack, and returns an object.

Finally, if you want to add a new POSIX character class, its handler
must start with "POSIX_".

=item $parser->del_handler(@handle_names)

This uninstalls the given handles.  You send the names (like '|' or
'atom').  Here is a very simple complete sub-class that does not allow
the C<(?{ ... })> and C<(??{ ... })> assertions:

  package Regexp::NoCode;
  use base 'Regexp::Parser';

  sub init {
    my $self = shift;
    $self->SUPER::init();
    $self->del_handler(qw<
      (?{   (??{   (?p{
    >);
  }

For those of you that don't know, C<(?p{ ... })> is a synonym for the
more common C<(??{ ... })>.  Using the 'p' form is deprecated, but is
still allowed, so I delete its handler too.  You can use this class to
ensure that there is are no code-execution statements in a regex:

  use Regexp::NoCode;
  my $p = Regexp::NoCode->new;

  # if it failed, reject it how you choose
  if (! $p->regex($regex)) {
    reject_regex(...);
  }

Any regex containing those assertions will fail to compile and throw an
error (specifically, B<RPe_NOTREC>, "Sequence (?xx not recognized").  If
you want to throw your own error, see L</"ERROR HANDLING">.

=back

=head2 Walking an Object

Most objects inherit their ender() and walk() methods from the base
object class; most have no ending node, and most don't need to to do
anything to the walking stack.

When an object does have an ending node, its ender() method should
return an array reference of arguments to object() that will produce
its ending node:

  # the 'open' node's ender:
  sub ender {
    my $self = shift;
    [ 'close', $self->nparen ];
  }

That means that when an 'open' node is walked into, after it has
been walk()ed, it will insert the matching 'close' node into the
walking stack.

The purpose of adding an ending node to the walking stack is that ending
nodes are all omitted from the tree because of the stacked nature of the
tree.  However, having them returned while I<walking> the tree is
helpful.

The walk() method is used to modify the walking stack before the node
is returned.  Here is the walk() method for all the quantifier and
'minmod' nodes:

  # star, plus, curly, minmod
  sub walk {
    my ($self, $walk_stack, $depth) = @_;
    unshift(@$walk_stack,
      sub { -1 },
      $self->{data},
      sub { +1 },
    ) if $depth;
  }

The two additional arguments sent are the walking stack and the current
depth in the walking stack.  Elements are taken from the I<front> of the
walking stack, so we add them in the order they are to be encountered
with unshift().  The two code references are used to go deeper and
shallower in scope; C<sub { -1 }> is used to go down into a deeper
scope, and C<sub{ +1 }> is used to come up out of it.  In between these
is C<< $self->{data} >>, which is the node's child.

=head2 Creating an Object

Ok, back to our F<Regexp::AndBranch> example.  Let me explain what the
'&' metacharacter will mean.  If you've used F<vim>, you might know
about its '\&' regex assertion.  It's an "AND", much like '|' is an
"OR".  The vim regex C</x\&y/> means "match I<y> if I<x> can be matched
at the same location".  Therefore it would be represented in Perl with a
look-ahead around the left-hand branch: C</(?=x)y/>.  We can expand this
to any number of branches: C</a\&b\&c\&d/> in vim would be
C</(?=a)(?=b)(?=c)d/> in Perl.  We will support this with the '&'
metacharacter.

We have added a handler for the '&' metacharacter, but now we need to
write the supporting class for the F<Regexp::AndBranch::and> object it
creates!

A method call for a F<Regexp::MyRx::THING> object will look in its own
package first, then in F<Regexp::MyRx::__object__> (if it exists), then
in F<Regexp::Parser::THING> (if it exists), and finally in
F<Regexp::Parser::__object__>.

Here is the definition of F<Regexp::AndBranch::and>:

  package Regexp::AndBranch::and;
  @ISA = qw( Regexp::Parser::branch );

  sub new {
    my ($class, $rx) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      data => [ [] ],
      family => 'branch',
      type => 'and',
      raw => '&',
      branch => 1,
    }, $class;
    return $self;
  }

We inherit the merge() method defined in F<Regexp::Parser::branch>,
which is used when two of the same node are matched in succession.  We
also inherit visual()Z and walk()Z<>.

However, we need to define our own qr() method, because we don't want
to have &'s in our I<real> regex.  We need to convert C<A&B&C> to
C<(?=A)(?=B)C>.

  # still in Regexp::AndBranch::and
  sub qr {
    my ($self) = @_;
    my @kids = @{ $self->{data} };

Here, @kids is an array that holds array references; each of those array
references is the body of one and-branch.  We will take the last one off
and keep it normal, but the others we will make to be look-aheads.  To
make an object, we need to access C<< $self->{rx} >>.

    my $consume = pop @kids;
    for (@kids) {
      $_ = $self->{rx}->object(ifmatch => 1, @$_);
    }

The 'ifmatch' object is a positive looking assertion, and the argument
of 1 means it's a look-ahead.  We send the unrolled contents of the
array reference as the contents of the look-ahead, and we're done.  Now
we just need to return the regex representation of our children:

    return join "",
      map($_->qr, @kids),
      map($_->qr, @$consume);
  }

Here's a sample execution:

  use Regexp::AndBranch;
  my $parser = Regexp::AndBranch->new;

  # matches the first number found in a string
  # that contains 'foo' somewhere init
  my $rx = q{^.*foo&\D*(\d+)};

  $parser->regex($rx) or die $parser->errmsg;
  print "VISUAL: ", $parser->visual, "\n";
  print "REGEX:  ", $parser->qr, "\n";

Here's the output:

  VISUAL: ^(?:.*foo&\D*(\d+))
  REGEX:  (?-xism:^(?:(?=.*foo)\D*(\d+)))

=head2 Extending the Extension

Here's a final example.  I'm going to rewrite F<Regexp::AndBranch> to
handle both '&' and '!'.  '!' will indicate a negative look-ahead.

  package Regexp::AndBranch;
  use base 'Regexp::Parser';

  sub init {
    my $self = shift;
    $self->SUPER::init();

    # X&Y = match Y if match X at the same place
    $self->add_handler('&' => sub {
      my ($S) = @_;
      push @{ $S->{next} }, qw< atom >;
      return $S->object(and => 1);
    });

    # X!Y = match Y unless match X at the same place
    $self->add_handler('!' => sub {
      my ($S) = @_;
      push @{ $S->{next} }, qw< atom >;
      return $S->object(and => 0);
    });
  }

We've added a handler, and added an argument to the constructor.  The
argument is a true or false value determining whether this is a positive
assertion.  Here's the new class for the object:

  package Regexp::AndBranch::and;
  @ISA = qw( Regexp::Parser::branch );

  sub new {
    my ($class, $rx, $pos) = @_;
    my $self = bless {
      rx => $rx,
      flags => $rx->{flags}[-1],
      data => [ [] ],
      family => 'branch',
      branch => 1,
      neg => !$pos,
    }, $class;
    return $self;
  }

We've added a C<neg> attribute, and removed both C<type> and C<raw>.  We
will replace them with methods:

  sub raw {
    my $self = shift;
    $self->{neg} ? '!' : '&';
  }

  sub type {
    my $self = shift;
    $self->{neg} ? 'not' : 'and';
  }

And finally, the small change to the qr() method:

  sub qr {
    my ($self) = @_;
    my @kids = @{ $self->{data} };
    my $consume = pop @kids;
    my $type = $self->{neg} ? 'unlessm' : 'ifmatch';

    for (@kids) {
      $_ = $self->{rx}->object($type => 1, @$_);
    }

    return join "",
      map($_->qr, @kids),
      map($_->qr, @$consume);
  }

Here's a sample run:

  use Regexp::AndBranch;
  my $parser = Regexp::AndBranch->new;

  my @RX = (
    q{^(?:.*foo&\D*(\d+))},
    q{^(?:.*foo!\D*(\d+))},
  );

  for (@RX) {
    $parser->regex($_) or die $parser->errmsg;
    print "VISUAL: ", $parser->visual, "\n";
    print "REGEX:  ", $parser->qr, "\n";
  }

The output is:

  VISUAL: ^(?:.*foo&\D*(\d+))
  REGEX:  (?-xism:^(?:(?=.*foo)\D*(\d+)))
  VISUAL: ^(?:.*foo!\D*(\d+))
  REGEX:  (?-xism:^(?:(?!.*foo)\D*(\d+)))

Presto!

=head2 Escape Sequences

If you are creating a new escape sequence, like '\y', your handler will
receive an additional argument which tells it if it's inside a character
class.

  $parser->add_handler('\y' => sub {
    my ($S, $cc) = @_;
    if ($cc) {
      # character class specific code
    }
    else {
      # elsewhere
    }
  });

=head2 Character Classes

Character classes are not returned all at once, but piece by piece.
Because range checking (C<[a-z]>) requires knowledge of the characters
on the lower and upper side of the range, objects must be created during
the first pass.  To accomplish this, use force_object(), which creates
an object regardless of what pass it's on.

  $parser->add_handler('\y' => sub {
    my ($S, $cc) = @_;
    if ($cc) {
      # so that $S->object(...) creates an object:
      $S->warn(RPe_BADESC, "y", " in character class");
      return $S->force_object(exact => "y");
    }
    else {
      # ...
    }
  });

Also note the C<RPe_BADESC> warning takes two arguments: the character
that was unexpectedly escaped, and a string.  If the warning is called
from a character class, pass " in character class"; otherwise, pass an
empty string.

=head1 ERROR HANDLING

=head2 Creating Custom Messages

It is probably easiest to follow my module when creating warning and
error messages for your sub-class.

  package Your::SubClass;
  # use constant NAME => VALUE, FMTSTRING
  use constant err_FOOBAR => 1, 'You broke the %s';
  use constant err_TOOBIG => 2, 'Regex too large';

Then you can access them via C<< $parser->err_FOOBAR >>, etc.

=head2 Throwing Warnings and Errors

There are three methods you can use when a problem arises.  They use
Carp::carp() or Carp::croak().  The argument list is used to fill in
the format string for sprintf().

=over 4

=item $parser->warn(RPe_ERRMSG, ARGS...)

Throws a warning I<only> during the first pass over the regex.

=item $parser->awarn(RPe_ERRMSG, ARGS...)

Unconditionally throws a warning.  Primarily useful when you need to
throw a warning that can only be figured out the second pass.

=item $parser->error(RPe_ERRMSG, ARGS...)

Throws a fatal error.

=back

=head1 SEE ALSO

L<Regexp::Parser>, L<Regexp::Parser::Objects>.

=head1 AUTHOR

Jeff C<japhy> Pinyan, F<japhy@perlmonk.org>

=head1 COPYRIGHT

Copyright (c) 2004 Jeff Pinyan F<japhy@perlmonk.org>. All rights reserved.
This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

