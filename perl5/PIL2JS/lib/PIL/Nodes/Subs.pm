use warnings;
use strict;

# Real subroutines, blocks, thunks.
# Back on days 1 and 2 of PIL2JS, the eval()s were needed to emulate Perl's
# "the return value of a sub is the value of the last expression evaluated":
#   (function () { 42 })()                 # undefined
#   (function () { return eval "42" })()   # 42
# Now PIL::PStmts wraps a return() around the last statement, but only if we're
# $IN_SUBLIKE. :)
{
  package PIL::PSub;

  sub as_js {
    my $self = shift;
    die unless @$self == 4;
    die if     ref $self->[0];
    die unless $self->[1]->isa("PIL::SubType");
    die unless ref($self->[2]) eq "ARRAY" or $self->[2]->isa("PIL::Params");
    bless $self->[2] => "PIL::Params";

    local $IN_SUBLIKE  = $self->[1]->as_constant;
    local $CUR_SUBNAME = $self->[0];

    warn "Skipping &*END.\n" and return "" if $CUR_SUBNAME eq "&*END";

    my $magical_vars = "";
    $magical_vars .= "_26main_3a_3a_3fBLOCK.STORE(%VAR);\n"
      if $IN_SUBLIKE >= PIL::SUBBLOCK;
    $magical_vars .= "_26main_3a_3a_3fSUB.STORE(%VAR);\n"
      if $IN_SUBLIKE >= PIL::SUBROUTINE;
    $magical_vars .= "_24main_3a_3a_3fSUBNAME.STORE(new PIL2JS.Box.Constant(%NAME));\n"
      if $IN_SUBLIKE >= PIL::SUBROUTINE;
    $magical_vars =~ s/%VAR/ PIL::name_mangle $self->[0]/eg;
    $magical_vars =~ s/%NAME/PIL::doublequote $CUR_SUBNAME/eg;

    my $pos_update =
      $self->[3]->isa("PIL::PPos")
    ? $self->[3]->[0]
    : $self->[3]->isa("PIL::PStmts") && $self->[3]->[0]->isa("PIL::PPos")
    ? $self->[3]->[0]->[0] : "";
    $magical_vars .=
      "_24main_3a_3a_3fPOSITION.STORE(new PIL2JS.Box.Constant(" .
      PIL::doublequote($pos_update) .
      "));\n" if $pos_update;

    # Subbody
    local $_;
    my $body = sprintf "%sPIL2JS.call_chain.push(%s);\n%s;\n%s;",
      $magical_vars,
      PIL::name_mangle($self->[0]),
      $self->[2]->as_js,
      $self->[3]->as_js;

    # Sub declaration
    my $js = sprintf
      "%s%s = PIL2JS.Box.constant_func(%d, function (args) {\n%s\n});\n",
      $IN_GLOBPIL ? "" : "var ",
      PIL::name_mangle($self->[0]),
      $self->[2]->arity,
      PIL::add_indent 1, PIL::generic_catch($IN_SUBLIKE, $body);
    $js .= sprintf
      "%s.perl_name = %s;\n",
      PIL::name_mangle($self->[0]),
      PIL::doublequote($self->[0]);

    # Special magic for methods.
    if($self->[1]->isa("PIL::SubMethod")) {
      my $methname = $self->[0];
      $methname = ($methname =~ /^&.*::(.+)$/)[0] or
        $FAIL->("Method names must be simple strings!");
      $js .= sprintf
        "PIL2JS.Box.prototype.perl_methods[%s] = %s;\n",
        PIL::doublequote($methname),
        PIL::name_mangle($self->[0]);
    }

    # Special magic for &*END_xyz subs.
    if($self->[0] =~ /^&\*END_\d+/) {
      $js .= sprintf
        "_40main_3a_3a_2aEND.FETCH().push(%s);\n",
        PIL::name_mangle $self->[0];
    }

    return $js;
  }
}

{
  package PIL::PCode;

  sub as_js {
    my $self = shift;

    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::SubType");
    die unless ref($self->[1]) eq "ARRAY" or $self->[1]->isa("PIL::Params");
    bless $self->[1] => "PIL::Params";

    local $IN_SUBLIKE  = $self->[0]->as_constant;
    local $CUR_SUBNAME = "<anonymous@{[$CUR_SUBNAME ? ' in ' . $CUR_SUBNAME : '']}>";

    # Subbody
    local $_;
    my $body = sprintf "%s;\n%s;",
      $self->[1]->as_js,
      $self->[2]->as_js;

    # Sub declaration
    return sprintf "PIL2JS.Box.constant_func(%d, function (args) {\n%s\n})",
      $self->[1]->arity,
      PIL::add_indent 1, PIL::generic_catch($IN_SUBLIKE, $body);
  }
}

{
  package PIL::PThunk;

  sub as_js {
    my $self = shift;
    local $IN_SUBLIKE  = PIL::SUBTHUNK;
    local $CUR_SUBNAME = "<thunk@{[$CUR_SUBNAME ? ' in ' . $CUR_SUBNAME : '']}>";

    die unless @$self == 1;

    local $_;
    return sprintf "PIL2JS.Box.constant_func(0, function (args) { var cxt = args.shift(); return(%s); })",
      $self->[0]->as_js;
  }
}

1;
