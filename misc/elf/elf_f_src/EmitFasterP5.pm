
class EmitFasterP5 is EmitSimpleP5 {

  method prelude_oo () {
    '
{package Object;
    our %DEFAULTS;
    sub new {
        my $self = shift;
        my $class = ref $self || $self;
        my $obj = bless {@_}, $class;
        for (keys %{$DEFAULTS{$class}}) {
            $obj->{$_} = $DEFAULTS{$class}{$_}->() unless $obj->{$_};
        }
        $obj;
    }
}
';
  };

  method cb__Trait ($n) {
    if ($n.verb eq 'is') {
      my $pkgname = $+whiteboard::in_package.join('::');
      my $name = $+whiteboard::in_package.splice(0,-1).join('::')~'::'~$.e($n.expr);
      "BEGIN{push(@"~$pkgname~"::ISA,'"~$name~"');}\n";
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n.verb~" has not been implemented.\n";
      "***Trait***"
    }
  };

  method using_Moose() {
      0;
  }
  method do_VarDecl_has ($n,$default) {
      my $name = $.e($n.var.name);
      my $code = "sub " ~ $name ~ ": lvalue {\n" ~
      '     $_[0]{' ~ "'" ~ $name ~ "'};\n" ~
      "}\n";
      if $default {
          $code = $code ~ '$Object::DEFAULTS{'~$+whiteboard::in_package.join('::')~"}{'"~ $name ~ "'} = sub {"~$default~'};';
      }
      $code;
  };

};
