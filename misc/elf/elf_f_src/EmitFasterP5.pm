
class EmitNoMooseP5 is EmitSimpleP5 {

  method prelude_oo () {
    '
{package Object;
    sub new {
        my $self = shift;
        my $class = ref $self || $self;
        if (ref $_[0] eq "HASH") {
            bless {%{$_[0]}}, $class;
        } else {
            bless {@_}, $class;
        }
    }
}
';
  };

  method cb__PackageDecl ($n) {

    my $^whiteboard::in_package = [$^whiteboard::in_package.flatten,$n<name>];
    my $name = $^whiteboard::in_package.join('::');
    my $base = "use base qw(Object);\n";
    if $name eq 'Object' { $base = "" }
    ("\n{ package "~$name~";\n"~
     $base~ 
     self.prelude_for_entering_a_package()~
     $.e($n<traits>||[]).join("\n")~
     $.e($n<block>)~
     "\n}\n");
  };
  method cb__Trait ($n) {
    if ($n<verb> eq 'is') {
      my $pkgname = $^whiteboard::in_package.join('::');
      my $name = $^whiteboard::in_package.splice(0,-1).join('::')~'::'~$.e($n<expr>);
      "BEGIN{push(@"~$pkgname~"::ISA,'"~$name~"');}\n";
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n<verb>~" has not been implemented.\n";
      "***Trait***"
    }
  };

  method do_VarDecl_has ($n) {
      my $default = $.e($n<default_expr>);
      if (defined $default) {
        $default = ", default => "~$default
      } else {
        $default = ""
      }
      "sub " ~ $.e($n<var><name>) ~ " {\n" ~
      "  if (@_ == 2) {\n" ~
      "      $_[0]{'" ~ $.e($n<var><name>) ~ "'} = $_[1];\n" ~
      " } else {\n" ~
      "     $_[0]{'" ~ $.e($n<var><name>) ~ "'};\n" ~
      "  }\n" ~
      "}\n";
  };

};

#if not($*emitter0) { $*emitter0 = EmitNoMooseP5.new}
$*emitter0 = EmitNoMooseP5.new;
$*emitter1 = EmitNoMooseP5.new;
