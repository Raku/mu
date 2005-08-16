# Parameters
{
  package PIL::Params;

  use warnings;
  use strict;

  sub fixup {
    my ($self, $subbody) = @_;
    local $_;

    return (
      pSubParams => (bless [ map { $_->fixup } @$self ] => "PIL::Params"),
      pSubBody   => $subbody->fixup,
    );
  }

  sub as_js {
    my $self = shift;
    local $_;

    # The parameter extracting thing is 3-pass.
    # Firstly, in as_js1, possible named args are immediately extracted and
    # removed from args.
    # Then, in as_js2, remaing positional args are picked up.
    # Finally,in as_js3, the actual checking is done.
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
      $_->type =~ /\b(?:Any|Junction|Bool)\b/
        ? "false"
        : do { $need_for_at++; "true" };
    } grep { $_->name ne '%_' } @$self;

    my $vars = join ", ", map { $_->name ne '%_' ? ($_->jsname) : () } @$self;

    if($need_for_at) {
      return sprintf "PIL2JS.possibly_autothread([%s], [%s], %s, function (%s) {\n%s\n});",
        $vars, join(", ", @bools), PIL::cur_retcc, PIL::cur_retcc . ", $vars", PIL::add_indent(1, $body);
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
  sub name        { $_[0]->{tpParam}{paramName} }
  sub type        { $_[0]->{tpParam}{paramContext}->[0]->[0] }
  sub fixed_name  { $_[0]->{fixedName} }
  sub jsname      { PIL::name_mangle $_[0]->fixed_name }

  sub fixup {
    my $self = shift;

    die unless defined $self->name and not ref $self->name;

    return bless {
      tpParam   => $self->{tpParam},
      tpDefault => $self->{tpDefault} && $self->{tpDefault}->fixup,
      fixedName => PIL::lookup_var $self->name,
    } => "PIL::MkTParam";
  }

  sub as_js0 {
    my $self = shift;
    warn "Skipping \%_ parameter.\n" and return "" if $self->name eq '%_';

    return sprintf "var %s = undefined;", $self->jsname;
  }

  sub as_js1 {
    my $self = shift;
    my $name = $self->name;
    warn "Skipping \%_ parameter.\n" and return "" if $name eq "%_";

    # If a param expects a real Pair, don't use it for named argument purposes.
    return "" if $_->type =~ /\b(?:Any|Pair)\b/;

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
    warn "Skipping \%_ parameter.\n" and return "" if $name eq "%_";

    # If we're a name-only arg, skip as_js2.
    #return "" if
    #  !$self->{tpParam}{isNamed}->isa("PIL::True") and not
    #  $name eq '$_';

    # It's a slurpy parameter? Flatten args so we can .shift() one item at a
    # time.
    my @js;
    if($self->{tpParam}{paramContext}->isa("PIL::CxtSlurpy")) {
      push @js, "args = PIL2JS.make_slurpy_array(args);";
    }

    my $jsname = $self->jsname;
    my $undef  = PIL::undef_of $name;
    # Are we a take-everything slurpy param (*@foo, as opposed to *$foo)?
    if(
      not $self->{tpParam}{paramContext}->isa("PIL::CxtSlurpy") or
      $name !~ /^@/
    ) {
      push @js, "if($jsname == undefined && args.length > 0) $jsname = $undef.BINDTO(args.shift());";
    } else {
      push @js, "if($jsname == undefined) { $jsname = new PIL2JS.Box.Constant(args); args = [] }";
    }

    return join "\n", @js;
  }

  sub as_js3 {
    my $self = shift;
    warn "Skipping \%_ parameter.\n" and return "" if $self->name eq "%_";

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
    warn "Skipping \%_ parameter.\n" and return "" if $self->name eq "%_";
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
