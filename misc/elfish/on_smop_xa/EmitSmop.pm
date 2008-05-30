
class EmitSmop {

  method new_emitter($ignore,$compiler) {
    self.new('compiler',$compiler);
  };

  has $.compiler;

  method prelude ($n) {
    ""
  };
  method e($x) {
    my $ref = $x.ref;
    if $ref eq 'UNDEF' { $x }
    elsif $ref eq 'SCALAR' { $x }
    elsif $ref eq 'ARRAY' { $x.map(sub($ae){$.e($ae)}) }
    else {$x.callback(self)}
  };
  method cb__CompUnit ($n) {
     $.e($n<statements>).join("\n")
  };
  method cb__MethodDecl ($n) {
    #'sub '~~'{my $self=CORE::shift;'~$.e($n<multisig>)~$.e($n<block>)~'}'
    my $name = $.e($n<name>);
    my $code = '
SMOP__Object* SMOP__S1P__method_NaMe;

void SMOP__S1P__method_NaMe_boot() {
  SMOP__Object* SMOP__ID__NaMe = SMOP__NATIVE__idconst_create("NaMe");
  NaMe = SMOP__OO__LOWL__Method_create(0, SMOP__ID__NaMe, SMOP__NATIVE__bool_false, SMOP__S1P__method_NaMe_run);
};

void SMOP__S1P__method_NaMe_run(SMOP__Object* interpreter, SMOP__Object* method, SMOP__Object* capture) {
';
    $code = $code.re_gsub('NaMe',$name);
    my $params = $n<multisig><parameters>;
    for $params {
      my $pname = $_<param_var><name>;
      $code = $code ~ '
  SMOP__Object* '~$pname~' = SMOP__NATIVE__capture_positional(interpreter,capture,0);
';
    }
    $code = $code ~ $.e($n<block>);
    $code = $code ~ '
  SMOP_DISPATCH(interpreter, interpreter, SMOP__ID__goto, frame);
};
';
    $code;
  };
  method cb__Block ($n) {
    ''~$.e($n<statements>).join(";\n")~";\n"
  };

  method cb__Signature ($n) {
    if($n<parameters>.elems == 0) { "" }
    else {
      'my('~$.e($n<parameters>).join(",")~')=@_;'~"\n";
    }
  };
  method cb__Parameter ($n) {
    $.e($n<param_var>)
  };
  method cb__ParamVar ($n) {
    $n<sigil>~$.e($n<name>)
  };
  
  method cb__NumInt ($n) {
    $.e($n<text>)
  };

};
$*emitter1 = EmitSmop.new;
