# Parameters
{
  package PIL::Params;

  use warnings;
  use strict;

  sub fixup {
    my ($self, $prefix, $subbody) = @_;
    local $_;

    # Move all slurpy arrays to the end.
    # This is because *%slurpy_hashes have to be processed before.
    my (@args, @slurpy_arrays, @invocant);
    my $seen_a_slurpy_param;

    for(@$self) {
      $_->is_first_slurpy++ if $_->is_slurpy and !$seen_a_slurpy_param++;
      if($_->is_invocant) {
        push @invocant, $_;

      # (Perl 5)--  # doesn't allow ?: as push argument
      } elsif($_->is_slurpy and $_->name =~ /^@/) {
          push @slurpy_arrays, $_;
      } else {
          push @args, $_;
      }
    }
    push    @args, @slurpy_arrays;
    unshift @args, @invocant;

    return (
      "p${prefix}Params" => (bless [ map { $_->fixup } @args ] => "PIL::Params"),
      "p${prefix}Body"   => $subbody->fixup,
    );
  }

  sub as_js {
    my $self = shift;
    local $_;

    # The parameter extracting thing is 3-pass.
    # In as_js0, appropriate "var %s;" declarations are emitted.
    # Then, in as_js1, possible named args are immediately extracted and
    # removed from args.
    # Iin as_js2, remaining positional args are picked up.
    # Finally, in as_js3, the actual checking is done.
    # In as_js_bind, the vars are then bound, and defaults are filled in, etc.
    my $js;
    $js .= "var cxt   = args.shift();\n";
    $js .= "args      = PIL2JS.possibly_flatten(args);\n";
    $js .= "var pairs = PIL2JS.grep_for_pairs(args);\n\n";
    $js .= $_->as_js0() . "\n" for @$self;
    $js .= $_->as_js1() . "\n" for @$self;
    $js .= $_->as_js2() . "\n" for @$self;
    $js .= $_->as_js3() . "\n" for @$self;
    chomp $js;
    $js .= <<EOF;
if(args.length != 0)
  PIL2JS.die("" + args.length + " more parameters passed to sub " + @{[PIL::doublequote $PIL::CUR_SUBNAME]} + " than expected (@{[scalar @$self]})!");
EOF

    return $js;
  }

  sub as_js_bind {
    my $self = shift;
    local $_;

    return join "\n", map { $_->as_js_bind } @$self;
  }

  sub autothread_wrapper {
    my ($self, $body) = @_;
    local $_;

    my $need_for_at;
    my @bools = map {
      !$_->is_slurpy and
      $_->type->matches("Any")      ||
      $_->type->matches("Junction") ||
      $_->type->matches("Bool")
        ? "false"
        : do { $need_for_at++; "true" };
    } @$self;

    my $vars = join ", ", map { $_->jsname } @$self;

    if($need_for_at) {
      return sprintf "PIL2JS.possibly_autothread([%s], [%s], %s, function (%s) {\n%s\n});",
        $vars,
        join(", ", @bools),
        PIL::cur_retcc,
        PIL::cur_retcc . ", $vars",
        PIL::add_indent(1, $body);
    } else {
      return $body;
    }
  }

  sub arity {
    local $_;
    my $arity = 0;
    $_->is_required and $arity++ for @{ $_[0] };
    return $arity;
  }
}

{
  package PIL::MkTParam;

  use warnings;
  use strict;

  sub is_required { not $_[0]->{tpParam}{isOptional} }
  sub is_invocant { $_[0]->{tpParam}{isInvocant} }
  sub name        { $_[0]->{tpParam}{paramName} }
  sub type        { $_[0]->{tpParam}{paramContext}->[0] }
  sub is_slurpy   { $_[0]->{tpParam}{paramContext}->isa("PIL::CxtSlurpy") }
  sub fixed_name  { $_[0]->{fixedName} }
  sub jsname      { PIL::name_mangle $_[0]->fixed_name }
  sub is_first_slurpy :lvalue { $_[0]->{isFirstSlurpy} }

  sub fixup {
    my $self = shift;

    die unless defined $self->name and not ref $self->name;

    return bless {
      tpParam       => $self->{tpParam},
      tpDefault     => $self->{tpDefault} && $self->{tpDefault}->fixup,
      isFirstSlurpy => $self->is_first_slurpy,
      fixedName     => PIL::lookup_var $self->name,
    } => "PIL::MkTParam";
  }

  sub as_js0 {
    my $self = shift;

    return sprintf "var %s = undefined;", $self->jsname;
  }

  sub as_js1 {
    my $self = shift;
    my $name = $self->name;

    # If a param expects a real Pair, don't use it for named argument purposes.
    return "" if
      $_->type->matches("Any") or $_->type->matches("Pair") or
      $_->is_invocant or $_->is_slurpy;
    # New change as of 2005-08-22:
    #   On 8/22/05, Larry Wall <larry@wall.org> wrote:
    #   > I think the simplest thing is to say that you can't bind to the name
    #   > of the slurpy
    #   hash.  You give a name to it so that you can refer to it
    #   > inside, but that name is not visible to binding.
    #
    #   Fixed in https://svn.perl.org/perl6/doc.  Thanks.
    #
    #   Luke

    my $jsname   = $self->jsname;
    my $pairname = PIL::doublequote substr $name, 1;
    my $undef    = PIL::undef_of $name;
    return substr <<EOF, 0, -1;  # cosmetical issue: strip the /\n$/.
if(pairs[$pairname] != undefined) {
  $jsname = $undef.BINDTO(pairs[$pairname]);
  args = PIL2JS.delete_pair_from_args(args, $pairname);
}
EOF
  }

  sub as_js2 {
    my $self = shift;
    my $name = $self->name;

    # If we're a name-only arg, skip as_js2.
    #return "" if
    #  !$self->{tpParam}{isNamed}->isa("PIL::True") and not
    #  $name eq '$_';

    # It's a slurpy parameter? Flatten args so we can .shift() one item at a
    # time.
    my @js;
    if($self->is_slurpy and $self->is_first_slurpy) {
      push @js, "args = PIL2JS.make_slurpy_array(args);";
    }

    my $jsname = $self->jsname;
    my $undef  = PIL::undef_of $name;
    # Are we a take-everything slurpy param (*@foo, as opposed to *$foo)?
    if(
      not $self->is_slurpy or
      $name =~ /^[\$\&]/
    ) {
      push @js,
        "if($jsname == undefined && args.length > 0) $jsname = $undef.BINDTO(args.shift());";
    } elsif($self->is_slurpy and $name =~ /^@/) {
      push @js,
        "if($jsname == undefined) { $jsname = new PIL2JS.Box.Constant(args); args = [] }";
    } elsif($self->is_slurpy and $name =~ /^%/) {
      push @js, (<<EOF =~ /^(.*)\n$/s)[0];
if($jsname == undefined) {
  var obj = PIL2JS.get_and_remove_all_pairs(args);
  args    = obj["args"];
  $jsname = new PIL2JS.Box.Constant(obj["hash"]);
}
EOF
    } else {
      die;
    }

    return join "\n", @js;
  }

  sub as_js3 {
    my $self = shift;

    # We're required, but a value hasn't been supplied?
    unless($self->{tpParam}{isOptional}) {
      return sprintf <<EOF, $self->jsname, map { PIL::doublequote($_) } $self->name, $PIL::CUR_SUBNAME;
if(%s == undefined)
  PIL2JS.die("Required parameter \\"" + %s + "\\" not passed to sub \\"" + %s + "\\"!");
EOF
    } else {
      return "";
    }
  }

  sub as_js_bind {
    my $self   = shift;
    my $name   = $self->name;
    my $jsname = $self->jsname;
    # - !perl/PIL::MkTParam
    # tpDefault: !perl/@PIL::Nothing []
    # tpParam: !perl/PIL::MkParam
    #   isInvocant: !perl/@PIL::False []
    #   isLValue: !perl/@PIL::True []
    #   isLazy: !perl/@PIL::False []
    #   isNamed: !perl/@PIL::True []
    #   isOptional: !perl/@PIL::False []
    #   isWritable: !perl/@PIL::False []
    #   paramContext: !perl/@PIL::CxtSlurpy
    #     - !perl/@PIL::mkType
    #       - Array
    #   paramDefault: !perl/@PIL::Noop []
    #   paramName: '@c'
    # Should we (and can we) supply a default for an optional param?
    my @js;

    if($self->{tpDefault}) {
      my $undef = PIL::undef_of $name;
      my $other = $self->{tpDefault};
      # XXX this will break, of course, if $other does call/cc magic.
      push @js,
        "if($jsname == undefined) PIL2JS.runloop(function () {" .
        PIL::possibly_ccify($other, PIL::Cont->new(
          argname => "val",
          body    => sub { "$jsname = val == undefined ? $undef : val;" },
        )) . "});";
    }

    # is copy?
    unless($self->{tpParam}{isLValue}) {
      push @js, "$jsname = $jsname.copy();";
    }

    # not is rw?
    unless($self->{tpParam}{isWritable}) {
      push @js, "$jsname = new PIL2JS.Box.ReadOnly($jsname);";
    }

    my $padname = PIL::doublequote $name;
    push @js, "pad[$padname] = $jsname;";

    # Always bind $?SELF to us, if we're the invocant param.
    if($self->{tpParam}{isInvocant} and $name ne '$?SELF') {
      push @js, sprintf "var %s = %s; pad[%s] = %s;",
        PIL::name_mangle('$?SELF'), $jsname,
        $padname,                   $jsname;
    }

    return join "\n", @js;
  }
}

1;
