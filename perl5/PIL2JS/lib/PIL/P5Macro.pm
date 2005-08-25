package PIL::P5Macro::JS;

use warnings;
use strict;

sub new {
  my ($class, $name, $body) = @_;

  return bless { body => $body, name => $name } => $class;
}

# XXX! TOTAL HACK!!!
# This is needed because perl5 seems to leak memory in the if-case below.
my $CORRECT_BEHAVIOUR = $ENV{PIL2JS_MACROS_CORRECT_BEHAVIOUR};

sub as_js {
  my ($self, @args) = @_;
  local $_;

  if($CORRECT_BEHAVIOUR) {
    my $jsname = PIL::name_mangle($self->{name});
    my @jsargs = map { "__args[$_]" } 0..$#args;

    return <<EOF;
var __cc   = $self->{CC};
var __args = [
@{[ join ",\n", map { PIL::add_indent 1, $_ } @args ]}
];
if($jsname.FETCH().is_still_our_pil2js_compiletime_macro) {
@{[ PIL::add_indent 1, $self->{body}->("__cc", @jsargs) ]}
} else {
  $jsname.FETCH()([PIL2JS.Context.ItemAny, @{[ join ", ", @jsargs ]}, __cc]);
}
EOF
  } else {
    return $self->{body}->($self->{CC}, @args);
  }
}

1;
