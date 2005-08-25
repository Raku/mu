package Prelude::JS;

use warnings;
use strict;

{
  our %macro;
  sub register { $macro{$_[0]} = PIL::P5Macro::JS->new($_[0] => $_[1]) }
}

{
  register "&statement_control:if" => my $if = sub {
    my ($cc, $cond, $true, $false) = @_;

    return <<EOF };
(
  PIL2JS.cps2normal(
    _26main_3a_3aprefix_3a_3f.FETCH(),
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
    [ "infix:/",  "N", "Number(a)  / Number(b)" ],
    [ "infix:%",  "N", "Number(a)  % Number(b)" ],
    [ "infix:**", "N", "Math.pow(Number(a), Number(b))" ],
  );

  for (@subs) {
    my ($name, $type, $body) = @$_;

    my $undef  = $type eq "S" ? '""' : 0;
    my $conv   = "_26main_3a_3aprefix_3a_" . ($type eq "S" ? "7e" : "2b");

    register "&$name" => sub {
      my ($cc, $a, $b) = @_;

      my %param = (a => $a, b => $b);

      my $jsbody = $body;
      $jsbody =~ s/\b([ab])\b/$1.toNative()/g;

      return "PIL2JS.possibly_autothread([PIL2JS.cps2normal($conv.FETCH(), [PIL2JS.Context.ItemAny, $a]), PIL2JS.cps2normal($conv.FETCH(), [PIL2JS.Context.ItemAny, $b])], [true, true], $cc, function (__cc, a, b) { __cc(new PIL2JS.Box.Constant($jsbody)) })";
    };
  }
}

1;
