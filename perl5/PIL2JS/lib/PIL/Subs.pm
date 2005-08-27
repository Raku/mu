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
  our @ISA = qw< PIL::PCode >;

  sub prefix { "Sub" }
  sub name   :lvalue { $_[0]->{pSubName}   }
  sub type   :lvalue { $_[0]->{pSubType}   }
  sub params :lvalue { $_[0]->{pSubParams} }
  sub lvalue :lvalue { $_[0]->{pSubLValue} }
  sub body   :lvalue { $_[0]->{pSubBody}   }

  sub fixup {
    die if ref $_[0]->name;
    return $_[0]->SUPER::fixup;
  }

  sub magical_vars {
    my $self = shift;

    my ($js, @vars) = $self->SUPER::magical_vars;
    my $jsvar       = PIL::name_mangle $self->name;
    my $name        = PIL::doublequote $self->name;
    $js .= "_26main_3a_3a_3fBLOCK = $jsvar; pad['&?BLOCK'] = _26main_3a_3a_3fBLOCK;\n"
      if $PIL::IN_SUBLIKE >= PIL::SUBBLOCK;
    $js .= "_26main_3a_3a_3fSUB = $jsvar; pad['&?SUB'] = _26main_3a_3a_3fSUB;\n"
      if $PIL::IN_SUBLIKE >= PIL::SUBROUTINE;
    $js .= "_24main_3a_3a_3fSUBNAME = new PIL2JS.Box.Constant($name); pad['\$?SUBNAME'] = _24main_3a_3a_3fSUBNAME;\n";

    return ($js, @vars, qw< &?BLOCK &?SUB $?SUBNAME >);
  }

  sub callchain {
    "PIL2JS_callchain.push(" . PIL::name_mangle($_[0]->name) . ");\n";
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::CUR_SUBNAME = $self->name;

    warn "Skipping &*END.\n"               and return ""
      if $self->name eq "&*END";
    warn "Skipping " . $self->name . ".\n" and return ""
      if $self->name =~ /^__export_c.*import$/;

    my $js = sprintf
      "%s%s = new PIL2JS.Box(%s.FETCH());\n%s.perl_name = %s;\n",
      $PIL::IN_GLOBPIL ? "" : "var ",
      PIL::name_mangle($self->name),
      $self->SUPER::as_js,
      PIL::name_mangle($self->name),
      PIL::doublequote($self->name);

    # Special magic for methods.
    if($self->type->isa("PIL::SubMethod")) {
      my $methname = $self->name;
      $methname = ($methname =~ /^&.*::(.+)$/)[0] or
        PIL::fail("Method names must be simple strings!");
      # method foo (A|B|C $self:) {...}
      my @classes = map { ":" . $_->as_string } $self->params->[0]->type->all_types;
      $js .= sprintf
        "PIL2JS.addmethod(%s, %s, %s);\n",
        PIL::name_mangle($_),
        PIL::doublequote($methname),
        PIL::name_mangle($self->name) for @classes;
    }

    # Special magic for &*END_xyz subs.
    if($self->name =~ /^&\*END_\d+/) {
      $js .= sprintf
        "_40main_3a_3a_2aEND.FETCH().push(%s);\n",
        PIL::name_mangle $self->name;
    }

    return $js;
  }

  sub unwrap { $_[0] }
}

{
  package PIL::PCode;

  sub prefix { "" }
  sub name   { "<anonymous@{[$PIL::CUR_SUBNAME ? ' in ' . $PIL::CUR_SUBNAME : '']}>" }
  sub type   :lvalue { $_[0]->{pType}   }
  sub params :lvalue { $_[0]->{pParams} }
  sub lvalue :lvalue { $_[0]->{pLValue} }
  sub body   :lvalue { $_[0]->{pBody}   }

  sub fixup {
    my $self = shift;

    die if     ref $self->type;
    die unless ref($self->params) eq "ARRAY";

    bless $self->params => "PIL::Params";
    $self->type = bless [] => "PIL::" . $self->type;  # minor hack

    local $PIL::IN_SUBLIKE  = $self->type->as_constant;
    local @PIL::IN_SUBLIKES = (@PIL::IN_SUBLIKES, $self->type->as_constant);

    # &PIL::Params::fixup returns the fixed PIL::Params and the fixed
    # $self->{pSubBody}.
    my %params_and_body = 
    return bless {
      $self->isa("PIL::PSub")
        ? (pSubName => $self->name)
        : (),
      "p" . $self->prefix . "Type"   => $self->type,
      "p" . $self->prefix . "LValue" => $self->lvalue,
      $self->params->fixup(
        $self->prefix,
        $self->body eq "PNil"
          ? bless {} => "PIL::PNil"
          : $self->body
      ),
    } => ref $self;
  }

  sub magical_vars {
    my $self = shift;

    my $vars;
    $vars .= $PIL::IN_SUBLIKE >= PIL::SUBROUTINE && !$self->isa("PIL::PSub")
      ? "_24main_3a_3a_3fSUBNAME = new PIL2JS.Box.Constant('<anon>');\n"
      : "";
    $vars .= "_24main_3a_3a_3fPOSITION = new PIL2JS.Box('<unknown>'); pad['\$?POSITION'] = _24main_3a_3a_3fPOSITION;\n";
    $vars .= "var _24main_3a_3a_ = new PIL2JS.Box(undefined); pad['\$_'] = _24main_3a_3a_;\n"
      unless grep { $_->name eq '$_' } @{ $self->params };

    # $?SUBNAME handled in PIL::PSub
    # We've to exclude $! from the list of vars-to-backup for primitives,
    # because else we can't implement &try in Perl.
    return (
      $vars,
      '$?POSITION',
      $PIL::IN_SUBLIKE == PIL::SUBPRIM ? () : '$!',
      !$self->isa("PIL::PSub") ? '$?SUBNAME' : ()
    );
  }

  sub callchain { "" }

  sub corofix {
    my ($self, $body) = @_;

    # Cosmetical fix
    chomp(my $ret = sprintf <<EOF, PIL::add_indent(1, $body), ($PIL::CORO_ID) x 3); ($ret, $PIL::CORO_ID++);
var initial_entrypoint = function () {
%s
};

if(!PIL2JS.coro_entrypoints[%d]) {
  PIL2JS.coro_entrypoints[%d] = initial_entrypoint;
}

PIL2JS.coro_entrypoints[%d](__returncc);
EOF
  }

  sub as_js {
    my $self = shift;
    local $_;

    local $PIL::IN_SUBLIKE  = $self->type->as_constant;
    local @PIL::IN_SUBLIKES = (@PIL::IN_SUBLIKES, $self->type->as_constant);
    local $PIL::CUR_SUBNAME = $self->name;

    my $callchain = $self->callchain;
    my $new_pad   = "var pad = {}; PIL2JS_subpads.push(pad)";
    my $params    = $self->params->as_js;
    (my $magical_vars, local @PIL::VARS_TO_BACKUP) = $self->magical_vars;

    my ($body, $coro_id) =
      $PIL::IN_SUBLIKE == PIL::SUBCOROUTINE
        ? $self->corofix($self->body->as_js)
        : $self->body->as_js;
    my $ccsetup     =
      $PIL::IN_SUBLIKE == PIL::SUBCOROUTINE
        ? PIL::coro_cc    $coro_id,       $self->lvalue, @PIL::VARS_TO_BACKUP
        : PIL::generic_cc PIL::cur_retcc, $self->lvalue, @PIL::VARS_TO_BACKUP;
    my $backup      = "var " . join ", ", map {
      sprintf "backup_%s = %s", PIL::name_mangle($_), PIL::name_mangle($_);
    } @PIL::VARS_TO_BACKUP;
    my $bind        = $self->params->as_js_bind;
    my $wrappedbody = "$new_pad;\n$callchain$magical_vars\n$bind;\n\n$body";

    my $jsbody = $params . "\n" . $self->params->autothread_wrapper($wrappedbody);

    return sprintf "PIL2JS.Box.constant_func(%d, function (args) {\n%s;\n%s;\n%s\n%s\n})",
      $self->params->arity,
      # Lexicalize PIL2JS and thus speed up PIL2JS
      PIL::add_indent(1, "var PIL2JS = AlsoPIL2JS_SpeedupHack"),
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
    local @PIL::IN_SUBLIKES = (@PIL::IN_SUBLIKES, PIL::SUBTHUNK);
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
