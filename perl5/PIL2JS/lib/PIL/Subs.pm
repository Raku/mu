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

    die unless @$self == 4;
    die if     ref $self->[0];
    die unless $self->[1]->isa("PIL::SubType");
    die unless ref($self->[2]) eq "ARRAY" or $self->[2]->isa("PIL::Params");
    bless $self->[2] => "PIL::Params";

    return bless [
      $self->[0],
      $self->[1],
      $self->[2]->fixup($self->[3]),
      # &PIL::Params::fixup returns the fixed PIL::Params and the fixed
      # $self->[3].
    ] => "PIL::PSub";
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::IN_SUBLIKE  = $self->[1]->as_constant;
    local $PIL::CUR_SUBNAME = $self->[0];

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

    # Subbody
    local @PIL::VARS_TO_BACKUP = ();
    my $body = sprintf "PIL2JS.call_chain.push(%s);\n%s;\n\n%s;\n\n%s;",
      PIL::name_mangle($self->[0]),
      $self->[2]->as_js,
      $magical_vars,
      $self->[3]->as_js;
    my $backup = "// Var backups:\nvar " . join ", ", map {
      sprintf "backup_%s = %s", PIL::name_mangle($_), PIL::name_mangle($_);
    } @PIL::VARS_TO_BACKUP, qw< &?BLOCK &?SUB $?SUBNAME >;
    $body = "$backup;\n\n$body";

    # Sub declaration
    my $js = sprintf
      "%s%s = PIL2JS.Box.constant_func(%d, function (args) {\n%s\n});\n",
      $PIL::IN_GLOBPIL ? "" : "var ",
      PIL::name_mangle($self->[0]),
      $self->[2]->arity,
      PIL::add_indent 1, PIL::generic_catch(
        $PIL::IN_SUBLIKE,
        $body,
        @PIL::VARS_TO_BACKUP, qw< &?BLOCK &?SUB $?SUBNAME >
      );
    $js .= sprintf
      "%s.perl_name = %s;\n",
      PIL::name_mangle($self->[0]),
      PIL::doublequote($self->[0]);

    # Special magic for methods.
    if($self->[1]->isa("PIL::SubMethod")) {
      my $methname = $self->[0];
      $methname = ($methname =~ /^&.*::(.+)$/)[0] or
        PIL::fail("Method names must be simple strings!");
      # method foo (A|B|C $self:) {...}
      my @classes = map { ":$_" } split /\|/, $self->[2]->[0]->type;
      $js .= sprintf
        "PIL2JS.addmethod(%s, %s, %s);\n",
        PIL::name_mangle($_),
        PIL::doublequote($methname),
        PIL::name_mangle($self->[0]) for @classes;
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

  sub fixup {
    my $self = shift;

    die unless @$self == 3;
    die unless $self->[0]->isa("PIL::SubType");
    die unless ref($self->[1]) eq "ARRAY" or $self->[1]->isa("PIL::Params");
    bless $self->[1] => "PIL::Params";

    return bless [
      $self->[0],
      $self->[1]->fixup($self->[2]),
      # &PIL::Params::fixup returns the fixed PIL::Params and the fixed
      # $self->[2].
    ] => "PIL::PCode";
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::IN_SUBLIKE  = $self->[0]->as_constant;
    local $PIL::CUR_SUBNAME = "<anonymous@{[$PIL::CUR_SUBNAME ? ' in ' . $PIL::CUR_SUBNAME : '']}>";

    # Subbody
    local @PIL::VARS_TO_BACKUP = ();
    my $body = sprintf "%s;\n\n%s%s;",
      $self->[1]->as_js,
      $PIL::IN_SUBLIKE >= PIL::SUBROUTINE
        ? "_24main_3a_3a_3fSUBNAME = new PIL2JS.Box.Constant('<anon>');\n\n"
        : "",
      $self->[2]->as_js;
    my $backup = "// Var backups:\nvar " . join ", ", map {
      sprintf "backup_%s = %s", PIL::name_mangle($_), PIL::name_mangle($_);
    } @PIL::VARS_TO_BACKUP, qw< $?SUBNAME >;
    $body = "$backup;\n$body";
    # Subbody

    # Sub declaration
    return sprintf "PIL2JS.Box.constant_func(%d, function (args) {\n%s\n})",
      $self->[1]->arity,
      PIL::add_indent 1, PIL::generic_catch(
        $PIL::IN_SUBLIKE, $body,
        @PIL::VARS_TO_BACKUP, qw< $?SUBNAME >
      );
  }
}

{
  package PIL::PThunk;

  sub fixup {
    my $self = shift;

    die unless @$self == 1;

    return bless [ $self->[0]->fixup ] => "PIL::PThunk";
  }

  sub as_js {
    my $self = shift;
    local $PIL::IN_SUBLIKE  = PIL::SUBTHUNK;
    local $PIL::CUR_SUBNAME = "<thunk@{[$PIL::CUR_SUBNAME ? ' in ' . $PIL::CUR_SUBNAME : '']}>";

    local $_;
    return sprintf "PIL2JS.Box.constant_func(0, function (args) { var cxt = args.shift(); return(%s); })",
      $self->[0]->as_js;
  }
}

1;
