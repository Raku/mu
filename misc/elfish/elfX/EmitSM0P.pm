
class EmitSM0P {

  method new_emitter($ignore,$compiler,$ignore2,$filename) {
    self.new('compiler',$compiler,'filename',$filename);
  };

  has $.SEQ = 1;
  has $.compiler;
  has $.filename;

  method tidy($source) {
      $source;
  }

  method prelude_lexical () {
      "# prelude lexical\n"
  }

  method prelude ($n) { "# prelude\n" }

  method e($x,$arg) {
    my $ref = $x.WHAT;
    if $ref eq 'Undef' { $x }
    elsif $ref eq 'Str' || $ref eq 'Int' || $ref eq 'Num' { $x }
    elsif $ref eq 'Array' { $x.map(sub($ae){$.e($ae,self.genid)}) }
    else {$x.callback(self,$arg)}
  }

  method genid () {
      # i'm not lazy to fix methods to be lvalues
      self.SEQ = self.SEQ + 1;
      return 'id'~self.SEQ;
  }
  method cb__CompUnit ($n) {
      "# sm0p code\n" ~ ";\n" ~ $.e($n.statements).join(';');
  }
  method capturize($n,$id) {
    my $positionals = $n.capture.arguments.keys.map(sub($k) {'`pos_'~$id}).join(',');
    "SMOP__SLIME__Capturize.new(`i_" ~ $id ~ ",(" ~ $positionals ~ "),(),1)";
  }
  method cb__Call ($n,$ret) {
      my $id = self.genid;
      my $positionals = "";
      for $n.capture.arguments.keys -> $i {
          $positionals = $positionals ~ $.e($n.capture.arguments[$i],"pos_"~$id);
      }
      "# method call " ~ $id ~ "\n" ~
      $.e($n.invocant,'i_'~$id) ~
      'ic_'~$id~': $SMOP__SLIME__CurrentFrame.copy(`i_' ~ $id ~ ");\n" ~
      'id_'~$id~':'~$n.method~";\n" ~
      $positionals ~
      '$SMOP__SLIME__CurrentFrame.move_responder(`ic_' ~ $id ~ ",3);\n" ~
      '$SMOP__SLIME__CurrentFrame.move_identifier(`id_' ~ $id ~ ",2);\n" ~
      '$SMOP__SLIME__CurrentFrame.move_capturize('~self.capturize($n,$id)~");\n" ~
      $ret ~ ": ;\n"
  };
  method cb__Buf ($n,$label) {
      $label ~ ":" ~ '"' ~ $n.buf.re_gsub(/\n/,"\\n") ~ '"' ~ ";\n"; #TODO: quote \ and "
  }
  method cb__Var ($n,$label) {
      $label ~ ': $' ~ $n.name ~ ";\n"
  };

}
