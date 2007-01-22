package Prelude::JS;

use warnings;
use strict;

{
  our %macro;
  sub register { $macro{$_[0]} = PIL::P5Macro::JS->new($_[0] => $_[1]) }
}
{
  register "&statement_control:cond" => sub {
    my ($cc, $cond, $true, $false) = @_;

    # XXX - Here we need to emit $true.() and $false.() instead of those two blocks.
    return <<EOF };
(
  PIL2JS.cps2normal(
    _26Main_3a_3aprefix_3a_3f.FETCH(),
    [ PIL2JS.Context.ItemAny, $cond ]
  ).FETCH()
    ?
@{[ PIL::add_indent 3, $true ]}
    :
@{[ PIL::add_indent 3, $false ]}
).FETCH()([
  PIL2JS.Context.ItemAny,
@{[ PIL::add_indent 1, $cc ]}
])
EOF
}

{
  register "&statement_control:if" => my $if = sub {
    my ($cc, $cond, $true, $false) = @_;

    return <<EOF };
(
  PIL2JS.cps2normal(
    _26Main_3a_3aprefix_3a_3f.FETCH(),
    [ PIL2JS.Context.ItemAny, $cond ]
  ).FETCH()
    ?
@{[ PIL::add_indent 3, $true ]}
    :
@{[ PIL::add_indent 3, $false ]}
).FETCH()([
  PIL2JS.Context.ItemAny,
@{[ PIL::add_indent 1, $cc ]}
])
EOF

  register "&statement_control:unless" => sub {
    my ($cc, $cond, $true, $false) = @_;

    return $if->($cc, $cond, $false, $true);
  };
}

{
  foreach my $name (qw<last next redo>) {
    register "&$name" => sub {
      my ($cc) = @_;

      # We purposefully *don't* include the cc.
      # Think "next; $other_code" -- $other_code has *no* possibility of being
      # called. (This does not affect "next if ...; $other_code".)
      return "(function () { throw new PIL2JS.ControlException.$name })()";
    };
  }
}

{
  register "&Class::_create" => sub {
    my ($cc, $classname) = @_;

    # Total hack.
    $classname =~ s/^.*?new PIL2JS.Box.Constant\((".*")\)$/eval $1/eg or die;

    push @PIL::PIL1_HACK_CLASSDECLS,
      sprintf "if(!%s) var %s = PIL2JS.new_empty_class(%s, _3aMain_3a_3aItem);",
        PIL::name_mangle(":$classname"),
        PIL::name_mangle(":$classname"),
        PIL::doublequote($classname);
    return "$cc(new PIL2JS.Box.Constant(undefined))";
  };
}

# Standard operators, taken from (v6) Prelude::JS::Operators
{
  my @subs = (
    [ "infix:<",  "N", "Number(a)  < Number(b)" ],
    [ "infix:>",  "N", "Number(a)  > Number(b)" ],
    [ "infix:<=", "N", "Number(a) <= Number(b)" ],
    [ "infix:>=", "N", "Number(a) >= Number(b)" ],
    [ "infix:==", "N", "Number(a) == Number(b)" ],
    [ "infix:!=", "N", "Number(a) != Number(b)" ],
    [ "infix:lt", "S", "String(a)  < String(b)" ],
    [ "infix:gt", "S", "String(a)  > String(b)" ],
    [ "infix:le", "S", "String(a) <= String(b)" ],
    [ "infix:ge", "S", "String(a) >= String(b)" ],
    [ "infix:eq", "S", "String(a) == String(b)" ],
    [ "infix:ne", "S", "String(a) != String(b)" ],
    [ "infix:+",  "N", "Number(a)  + Number(b)" ],
    [ "infix:-",  "N", "Number(a)  - Number(b)" ],
    [ "infix:*",  "N", "Number(a)  * Number(b)" ],
    [ "infix:/",  "N", "Number(b) == 0 ? eval('throw(new Error(\\'Division by zero\\'))') : Number(a)  / Number(b)" ],
    [ "infix:%",  "N", "Number(b) == 0 ? eval('throw(new Error(\\'Modulo zero\\'))') : Number(a)  % Number(b)" ],
    [ "infix:**", "N", "Math.pow(Number(a), Number(b))" ],
  );

  for (@subs) {
    my ($name, $type, $body) = @$_;

    my $undef  = $type eq "S" ? '""' : 0;
    my $conv   = "_26Main_3a_3aprefix_3a_" . ($type eq "S" ? "7e" : "2b");

    register "&$name" => sub {
      my ($cc, $a, $b) = @_;

      my %param = (a => $a, b => $b);

      my $jsbody = $body;
      $jsbody =~ s/\b([ab])\b/PIL2JS.cps2normal($conv.FETCH(), [PIL2JS.Context.ItemAny, $1]).toNative()/g;
      # Previously, "|| $undef" was required, This is no longer the case, as
      # &prefix:<[+~]> always return 0 or "" on undefined.
      # "Why not simply make +undef return undef?" -- Because then all
      # operations yield NaN, which doesn't match Perl's semantics.

      return "PIL2JS.possibly_autothread([$a, $b], [true, true], $cc, function (__cc, a, b) { __cc(new PIL2JS.Box.Constant($jsbody)) })";
    };
  }
}

1;
