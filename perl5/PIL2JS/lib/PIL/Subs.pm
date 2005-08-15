use warnings;
use strict;

# Real subroutines, blocks, thunks.
# Back on days 1 and 2 of PIL2JS, the eval()s were needed to emulate Perl's
# "the return value of a sub is the value of the last expression evaluated":
#   (function () { 42 })()                 # undefined
#   (function () { return eval "42" })()   # 42
# Now PIL::PStmts wraps a return() around the last statement, but only if we're
# $PIL::IN_SUBLIKE. Easy, eh? :)
{
  package PIL::PSub;

  sub fixup {
    my $self = shift;

    die unless keys %$self == 4;
    die if     ref $self->{pSubName};
    die unless $self->{pSubType};
    die unless ref($self->{pSubParams}) eq "ARRAY";

    bless $self->{pSubParams} => "PIL::Params";
    bless $self->{pSubType}   => $self->{pSubType};  # minor hack

    local $PIL::IN_SUBLIKE = $self->{pSubType}->as_constant;

    return bless {
      pSubName => $self->{pSubName},
      pSubType => $self->{pSubType},
      $self->{pSubParams}->fixup(
        $self->{pSubBody} eq "PNil"
          ? bless {} => "PIL::PNil"
          : $self->{pSubBody}
      ),
      # &PIL::Params::fixup returns the fixed PIL::Params and the fixed
      # $self->{pSubBody}.
    } => "PIL::PSub";
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::IN_SUBLIKE  = $self->{pSubType}->as_constant;
    local $PIL::CUR_SUBNAME = $self->{pSubName};

    warn "Skipping &*END.\n"      and return "" if $PIL::CUR_SUBNAME eq "&*END";
    warn "Skipping $self->[0].\n" and return ""
      if $PIL::CUR_SUBNAME =~ /^__export_c.*import$/;

    my $magical_vars = "";
    $magical_vars .= "_26main_3a_3a_3fBLOCK = %VAR;\n"
      if $PIL::IN_SUBLIKE >= PIL::SUBBLOCK;
    $magical_vars .= "_26main_3a_3a_3fSUB = %VAR;\n"
      if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
    $magical_vars .= "_24main_3a_3a_3fSUBNAME = new PIL2JS.Box.Constant(%NAME);\n"
      if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
    $magical_vars =~ s/%VAR/ PIL::name_mangle $self->[0]/eg;
    $magical_vars =~ s/%NAME/PIL::doublequote $PIL::CUR_SUBNAME/eg;

    my $callchain   = "PIL2JS.call_chain.push(" . PIL::name_mangle($self->[0]) . ")";
    my $new_pad     = "var pad = {}; PIL2JS.subpads.push(pad)";
    my $params      = $self->{pSubParams}->as_js;

    local @PIL::VARS_TO_BACKUP = ();
    my $body        = $self->{pSubBody}->as_js;
    my $ccsetup     = PIL::generic_cc PIL::cur_retcc, @PIL::VARS_TO_BACKUP, qw< &?BLOCK &?SUB $?SUBNAME >;
    my $backup      = "var " . join ", ", map {
      sprintf "backup_%s = %s", PIL::name_mangle($_), PIL::name_mangle($_);
    } @PIL::VARS_TO_BACKUP, qw< &?BLOCK &?SUB $?SUBNAME >;
    my $bind        = $self->{pSubParams}->as_js_bind;
    my $wrappedbody = "$new_pad;\n$callchain;\n$magical_vars;\n\n$bind;\n\n$body";

    my $jsbody = $params . "\n" . $self->{pSubParams}->autothread_wrapper($wrappedbody);

    # Sub declaration
    my $js = sprintf
      "%s%s = PIL2JS.Box.constant_func(%d, function (args) {\n%s;\n%s\n%s\n});\n",
      $PIL::IN_GLOBPIL ? "" : "var ",
      PIL::name_mangle($self->{pSubName}),
      $self->{pSubParams}->arity,
      PIL::add_indent(1, $backup),
      PIL::add_indent(1, $ccsetup),
      PIL::add_indent(1, $jsbody);
    $js .= sprintf
      "%s.perl_name = %s;\n",
      PIL::name_mangle($self->{pSubName}),
      PIL::doublequote($self->{pSubName});

    # Special magic for methods.
    if($self->{pSubType}->isa("PIL::SubMethod")) {
      my $methname = $self->{pSubName};
      $methname = ($methname =~ /^&.*::(.+)$/)[0] or
        PIL::fail("Method names must be simple strings!");
      # method foo (A|B|C $self:) {...}
      my @classes = map { ":$_" } split /\|/, $self->{pSubParams}[0]->type;
      $js .= sprintf
        "PIL2JS.addmethod(%s, %s, %s);\n",
        PIL::name_mangle($_),
        PIL::doublequote($methname),
        PIL::name_mangle($self->{pSubName}) for @classes;
    }

    # Special magic for &*END_xyz subs.
    if($self->{pSubName} =~ /^&\*END_\d+/) {
      $js .= sprintf
        "_40main_3a_3a_2aEND.FETCH().push(%s);\n",
        PIL::name_mangle $self->{pSubName};
    }

    return $js;
  }

  sub unwrap { $_[0] }
}

{
  package PIL::PCode;

  sub fixup {
    my $self = shift;

    die unless keys %$self == 3;
    die if     ref $self->{pSubType};
    die unless ref($self->{pSubParams}) eq "ARRAY";

    bless $self->{pSubParams} => "PIL::Params";
    bless $self->{pSubType}   => $self->{pSubType};  # minor hack

    local $PIL::IN_SUBLIKE = $self->{pSubType}->as_constant;

    return bless {
      pSubName => $self->{pSubName},
      $self->{pSubParams}->fixup($self->{pSubBody}),
      # &PIL::Params::fixup returns the fixed PIL::Params and the fixed
      # $self->{pSubBody}.
    } => "PIL::PCode";
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::IN_SUBLIKE  = $self->{pSubType}->as_constant;
    local $PIL::CUR_SUBNAME = "<anonymous@{[$PIL::CUR_SUBNAME ? ' in ' . $PIL::CUR_SUBNAME : '']}>";

    my $new_pad     = "var pad = {}; PIL2JS.subpads.push(pad)";
    my $params      = $self->{pSubParams}->as_js;
    my $magical_var = $PIL::IN_SUBLIKE >= PIL::SUBROUTINE
      ? "_24main_3a_3a_3fSUBNAME = new PIL2JS.Box.Constant('<anon>');\n\n"
      : "";

    local @PIL::VARS_TO_BACKUP = ();
    my $body        = $self->{pSubBody}->as_js;
    my $ccsetup     = PIL::generic_cc PIL::cur_retcc, @PIL::VARS_TO_BACKUP, qw< $?SUBNAME >;
    my $backup      = "var " . join ", ", map {
      sprintf "backup_%s = %s", PIL::name_mangle($_), PIL::name_mangle($_);
    } @PIL::VARS_TO_BACKUP, qw< $?SUBNAME >;
    my $bind        = $self->{pSubParams}->as_js_bind;
    my $wrappedbody = "$new_pad;\n$magical_var$bind;\n\n$body";

    my $jsbody = $params . "\n" . $self->{pSubParams}->autothread_wrapper($wrappedbody);

    return sprintf "PIL2JS.Box.constant_func(%d, function (args) {\n%s;\n%s\n%s\n})",
      $self->{pSubParams}->arity,
      PIL::add_indent(1, $backup),
      PIL::add_indent(1, $ccsetup),
      PIL::add_indent(1, $jsbody);
  }

  sub unwrap { $_[0] }
}

{
  package PIL::PThunk;

  sub fixup {
    my $self = shift;

    die unless keys %$self == 1;

    return bless { (%$self)[0] => (%$self)[1]->fixup } => "PIL::PThunk";
  }

  sub as_js {
    my $self = shift;
    local $PIL::IN_SUBLIKE  = PIL::SUBTHUNK;
    local $PIL::CUR_SUBNAME = "<thunk@{[$PIL::CUR_SUBNAME ? ' in ' . $PIL::CUR_SUBNAME : '']}>";

    my $body = PIL::possibly_ccify +(%$self)[1], PIL::RawJS->new("thunkreturncc");
    my $ret  = sprintf <<EOF, PIL::add_indent 1, $body;
PIL2JS.Box.constant_func(0, function (args) {
  var cxt           = args.shift();
  var thunkreturncc = args.pop();
%s;
})
EOF

    chomp $ret;  # Cosmetical fix
    return $ret;
  }

  sub unwrap { $_[0] }
}

1;
