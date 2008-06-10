
class EmitSBCL {

  method new_emitter($ignore,$compiler,$ignore2,$filename) {
    self.new('compiler',$compiler,'filename',$filename);
  };

  has $.compiler;
  has $.filename;

  method tidy($source) { $source }

  method prelude_for_entering_a_package () {
    "";
  };

  method prelude_lexical () {
    "";
  };

  method prelude_oo () {
    '
';
  };
  method prelude ($n) {
  '#|
#fasl=`dirname $0`/`basename $0 .lisp`.fasl
#[ $fasl -ot $0 ] && sbcl --noinform --eval "(compile-file \"$0\")" --eval "(quit)"
#exec sbcl --noinform --load $fasl --end-toplevel-options "$@"
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
'~self.prelude_oo~self.prelude_lexical~'

(defun GLOBAL__say (x) (if (stringp x) (write-string x) (write x)) (format t "~%"))

';
  };

  method e($x) {
    my $ref = $x.WHAT;
    if $ref eq 'Undef' { $x }
    elsif $ref eq 'Str' || $ref eq 'Int' || $ref eq 'Num' { $x }
    elsif $ref eq 'Array' { $x.map(sub($ae){$.e($ae)}) }
    else {$x.callback(self)}
  };


  method cb__CompUnit ($n) {
    $n.do_all_analysis();
    my $+whiteboard::in_package = [];
    my $+whiteboard::emit_pairs_inline = 0;
    my $+whiteboard::compunit_footer = ['(quit)'];
    my $code = (
      ";;; todo: -> Main\n"~
      self.prelude_for_entering_a_package());
    my $stmts = $.e($n.statements);
    my $foot = $+whiteboard::compunit_footer.join("\n");
    $code ~ $stmts.join("\n")~$foot~"\n";
  };
  method cb__Block ($n) {
    my $+whiteboard::emit_pairs_inline = 0;
    #'# '~$.e($n.notes<lexical_variable_decls>).join(" ")~"\n"~
    '(progn '~$.e($n.statements).join("\n")~')'
  };

  method cb__Use ($n) {
    my $module = $.e($n.module_name);
    my $expr = $.e($n.expr);
    if $module eq 'v6-alpha' { "" }
    elsif $module eq 'v6' { "" }
    elsif $module eq 'lib' {
      my $name = $n.expr.buf;
      if $.compiler.hook_for_use_lib($name) { "" }
      else { "" }
    }
    elsif $.compiler.hook_for_use($module,$expr) { "" }
    else {
      "use " ~$module;
    }
  };
  method cb__ClosureTrait ($n) {
    my $+whiteboard::emit_pairs_inline = 0;
    $n.kind~'{'~$.e($n.block)~'}'
  };

  method cb__PackageDecl ($n) {
    my $in_package = [$+whiteboard::in_package.flatten,$n.name];
    my $name = $in_package.join('::');
    my $base = 'use base "Any";';
    if $name eq 'Any' { $base = '' }
    if $name eq 'Object' { $base = '' }
    if $name eq 'Junction' { $base = '' }
    my $head = "\n{ package "~$name~";\n";
    my $foot = "\n}\n";
    if $.using_Moose {
       $head = $head ~ "use Moose;"~" __PACKAGE__->meta->make_mutable();\n";
       $foot = ";\n__PACKAGE__->meta->make_immutable();\n"~ "\n}\n";
    }
    $head = $head ~ $base~ self.prelude_for_entering_a_package();
    if $n.block {
      my $+whiteboard::in_package = $in_package; # my()
      $head ~ $.e($n.traits||[]).join("\n") ~ $.e($n.block) ~ $foot;
    } else {
      $+whiteboard::in_package = $in_package; # not my()
      $+whiteboard::compunit_footer.unshift($foot);
      $head ~ $.e($n.traits||[]).join("\n") ~ ";\n"
    }
  };
  method cb__Trait ($n) {
    if ($n.verb eq 'is') {
      my $pkgname = $+whiteboard::in_package.join('::');
      my $name = $+whiteboard::in_package.splice(0,-1).join('::')~'::'~$.e($n.expr);
      $name.re_gsub('^::','');
      "BEGIN{push(@"~$pkgname~"::ISA,'"~$name~"');}\n";
    } else {
      say "ERROR: Emitting p5 for Trait verb "~$n.verb~" has not been implemented.\n";
      "***Trait***"
    }
  };

  method do_VarDecl_has ($n,$default) {
      if ($default) {
          $default = ", default => sub{ "~$default~" }"
      }
      "has '"~$.e($n.var.name)~"' => (is => 'rw'"~$default~");"
  };

  method cb__VarDecl ($n) {
    my $+whiteboard::emit_pairs_inline = 0;
    if ($n.scope eq 'has') {
      my $default = "";
      my $default_expr = $.e($n.default_expr);
      if $default_expr {
        $default = $default_expr;
      } else {
        if ($n.var.sigil eq '@') { $default = '[]' }
        if ($n.var.sigil eq '%') { $default = '{}' }
      }
      self.do_VarDecl_has($n,$default);
    } else {
      my $default = "";
      if $n.default_expr {
        if (not($n.var.sigil eq '$') &&
            $n.default_expr.isa('IRx1::Apply') &&
            ($n.default_expr.function eq 'circumfix:( )' ||
             $n.default_expr.function eq 'infix:,'))
        {
          my $pre = ''; my $post = '';
          if $n.is_array { $pre = '['; $post = ']' }
          if $n.is_hash  { $pre = '{'; $post = '}' }
          my $+whiteboard::emit_pairs_inline = 1;
          $default = ' = '~$pre~$.e($n.default_expr)~$post;
        } else {
          $default = ' = '~$.e($n.default_expr);
        }
      } else {
        if ($n.var.sigil eq '@') { $default = ' = [];' }
        if ($n.var.sigil eq '%') { $default = ' = {};' }
      }
      if ($n.is_context) {
        # HACK 
        my $name = $.e($n.var);
        $name.re_sub_g('^(.)::','$1');
        ("{package main; use vars '"~$name~"'};"~
         'local'~' '~$.e($n.var)~$default)
      }
      else {
        $n.scope~' '~$.e($n.var)~$default
      }
    }
  };


  method multimethods_using_hack ($n,$name,$param_types) {
    my $name = $.e($n.name);
    my $param_types = $n.multisig.parameters.map(sub($p){
      my $types = $.e($p.type_constraints);
        if $types {
          if $types.elems != 1 { die("unsupported: parameter with !=1 type constraint.") }
          $types[0];
        } else {
          undef;
        }
    });
    my $type0 = $param_types[0];
    if not($type0) {
      die("implementation limitation: a multi method's first parameter must have a type: "~$name~"\n");
    }
    my $stem = '_mmd__'~$name~'__';
    my $branch_name = $stem~$type0;
    my $setup_name = '_reset'~$stem;
    my $code = "";
    $code = $code ~
    '
{ my $setup = sub {
    my @meths = __PACKAGE__->meta->compute_all_applicable_methods;
    my $h = {};
    for my $m (@meths) {
      next if not $m->{name} =~ /^'~$stem~'(\w+)/;
      my $type = $1;
      $h->{$type} = $m->{code}{q{&!body}};
    };
    my $s = eval q{sub {
      my $ref = ref($_[1]) || $_[1]->WHAT;
      my $f = $h->{$ref}; goto $f if $f;
      Carp::croak "multi method '~$name~' cant dispatch on type: ".$ref."\n";
    }};
    die $@ if $@;
    eval q{{no warnings; *'~$name~' = $s;}};
    die $@ if $@;
    goto &'~$name~';
  };
  eval q{{no warnings; *'~$setup_name~' = $setup;}};
  die $@ if $@;
  eval q{{no warnings; *'~$name~' = $setup;}};
  die $@ if $@;
};
';
    'sub '~$branch_name~'{my $self=CORE::shift;'~$.e($n.multisig)~$.e($n.block)~'}' ~ $code;
  };
  method multi_using_CM ($n,$is_method,$f_emitted) {
    my $name = $.e($n.name);
    my $param_types = $n.multisig.parameters.map(sub($p){
      my $types = $.e($p.type_constraints);
      if $types {
        if $types.elems != 1 { die("unsupported: parameter with !=1 type constraint.") }
        $types[0];
      } else {
        'Any'
      }
    });
    if $is_method {
      $param_types.unshift('Any');
    }
    my $sig = $param_types.map(sub($t){
      # XXX C::M needs to be modified to work on both INTEGER and Int. :(
      if $t eq 'Any' { '*' }
      elsif $t eq 'Int' { '#' }
      elsif $t eq 'Num' { '#' }
      elsif $t eq 'Str' { '$' }
      else { $t }
    }).join(' ');
    'Class::Multimethods::multimethod '~$name~
    " => split(/\s+/,'"~$sig~"') => "~ $f_emitted ~';';
  };
  method cb__MethodDecl ($n) {
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'p5' {
      $body = $n.block.statements[0].buf;
    }
    else {
      $body = $.e($n.block);
    }
    if $n.plurality && $n.plurality eq 'multi' {
      #self.multimethods_using_hack($n);
      my $ef = 'sub {my $self=CORE::shift;'~$.e($n.multisig)~$body~'}';
      self.multi_using_CM($n,1,$ef);
    }
    else {
      'sub '~$.e($n.name)~'{my $self=CORE::shift;'~$.e($n.multisig)~$body~'}';
    }
  };

  method cb__SubDecl ($n) {
    my $+whiteboard::emit_pairs_inline = 0;
    my $name = $n.name;
    if $name { $name = $.e($name) } else { $name = "" }
    my $sig = $n.multisig;
    if $sig { $sig = $.e($sig) } else { $sig = "" }
    my $body;
    if $n.traits && $n.traits[0].expr && $n.traits[0].expr eq 'p5' {
      $body = $n.block.statements[0].buf;
    } else {
      $body = $.e($n.block);
    }
    if $n.plurality && $n.plurality eq 'multi' {
      my $ef = 'sub {'~$sig~$body~'}';
      self.multi_using_CM($n,0,$ef);
    } else {
      '(defun '~$name~' ('~$sig~') '~$body~')';
    }
  };
  method cb__Signature ($n) {
    if ($n.parameters.elems == 0) { "" }
    else {
      my $+whiteboard::signature_inits = "";
      my $pl = $.e($n.parameters).join(" ");
      ''~$pl~''~$+whiteboard::signature_inits~"\n";
    }
  };
  method cb__Parameter ($n) {
    my $enc = $.e($n.param_var);
    if $n.quant && $n.quant eq '*' {
      my $tmp = "@"~$n.param_var.name;
      $+whiteboard::signature_inits = $+whiteboard::signature_inits~"\nmy "~$enc~" = \\"~$tmp~";";
      $tmp;
    } else {
      $enc;
    }
  };
  method cb__ParamVar ($n) {
    my $s = $n.sigil;
    my $t = '';
    my $dsn = $.e($n.name);
    $.encode_varname($s,$t,$dsn);
  };

  method cb__Call ($n) {
    my $+whiteboard::emit_pairs_inline = 0;
    my $method = $.e($n.method);
    if ($method =~ 'postcircumfix:< >') {
      $.e($n.invocant)~'->'~"{'"~$.e($n.capture)~"'}";
    }
    elsif ($method =~ 'postcircumfix:(.*)') {
      my $op = $1;
      my $arg = $.e($n.capture);
      $op.re_gsub(' ',$arg);
      $.e($n.invocant)~'->'~$op;
    } else {
      $.e($n.invocant)~'->'~$.e($n.method)~'('~$.e($n.capture)~')'
    }
  };
  method using_Moose() {
      # $foo.bar = ... turned into $foo.bar(...)
      # Moose-based class initializers.
      1;
  }
  method cb__Apply ($n) {
    # my $+whiteboard::emit_pairs_inline = 0; #XXX depends on function :/
    my $fun = $.e($n.function);
    if $fun =~ /^infix:(.+)$/ {
      my $op = $1;
      my $args = $n.capture.arguments;
      my $a = $.e($args);
      my $l = $a[0];
      my $r = $a[1];
      if ($op eq '~') {
        return "("~$l~" . "~$r~")"
      }
      if ($op eq ',') {
        my $s = $a.shift;
        while $a.elems { $s = $s ~" "~ $a.shift }
        return $s;
      }
      if ($op eq '=') && $.using_Moose {
        if $args[0].isa("IRx1::Var") {
          my $t = $args[0].twigil;
          if ($t && $t eq '.') {
            return $l~'('~$r~')'
          }
        }
        if ($args[0].isa("IRx1::Call") &&
            $args[0].capture.arguments.elems == 0)
        {
          return $.e($args[0].invocant)~'->'~$.e($args[0].method)~'('~$r~')'
        }
      }
      "("~$op~" "~$l~" "~$r~")";
    }
    elsif $fun =~ /^prefix:(.+)$/ {
      my $op = $1;
      my $a = $.e($n.capture.arguments);
      my $x = $a[0];
      if 0 { }
      elsif $op eq '?' { '(('~$x~')?1:0)' }
      elsif $op eq '+' { 'GLOBAL::prefix_plus('~$x~')' }
      else { "("~$op~""~$x~")" }
    }
    elsif $fun =~ /^statement_prefix:(.+)$/ {
      my $op = $1;
      if $op eq 'do' {
        'do{'~$.e($n.capture.arguments[0])~'}'
      } elsif $op eq 'try' {
        'eval{'~$.e($n.capture)~'}'
      } elsif $op eq 'gather' {
        'GLOBAL::gather'~$.e($n.capture)~''
      } else {
        die $fun~': unimplemented';
      }
    }
    elsif $fun =~ /^postfix:(.+)$/ {
      my $op = $1;
      my $a = $.e($n.capture.arguments);
      my $x = $a[0];
      if 0 { }
      else { "("~$x~""~$op~")" }
    }
    elsif $fun =~ /^circumfix:(.+)/ {
      my $op = $1;
      if $op eq '< >' {
        my $s = $n.capture.arguments[0];
        my $words = $s.split(/\s+/);
        if $words.elems == 0 {
          '[]'
        } else {
          "['"~$words.join("','")~"']"
        }
      } else {
        my $arg = $.e($n.capture);
        $op.re_gsub(' ',$arg);
      }
    }
    else {
      my $f = $fun;
      if ($f =~ /^\$\w+$/) {
         $f~'->('~$.e($n.capture)~')';
      } elsif ($f eq 'self') {
        '$self'
      } elsif ($f eq 'last') {
        'last'
      } elsif ($f eq 'return') {
        'return('~$.e($n.capture)~')';
      } elsif ($f =~ /^sub\s*{/) {
        '('~$f~')->('~$.e($n.capture)~')'
      } elsif ($f =~ /^\w/) {
        if $n.notes<lexical_bindings>{'&'~$f} {
          '('~$f~' '~$.e($n.capture)~')'
        } else {
          if $f eq 'eval' {
            my $env = 'sub{my$s=eval($_[0]);Carp::carp($@)if$@;$s}';
            'GLOBAL::'~$f~'('~$.e($n.capture)~','~$env~')'
          } else {
            '(GLOBAL__'~$f~' '~$.e($n.capture)~')'
          }
        }
      } else {
         $f~'('~$.e($n.capture)~')';
      }
    }
  };
  method cb__Capture ($n) {
    # my $+whiteboard::emit_pairs_inline = 0; XXX?
    my $a = $.e($n.arguments||[]).join(" ");
    if $n.invocant {
      my $inv = $.e($n.invocant);
      if $a { $inv~" "~$a }
      else { $inv }
    }
    else { $a }
  };

  method cb__For ($n) {
    my $push = "";
    if $n.expr.WHAT ne 'IRx1::Apply' { $push = "->flatten"};
    my $pull = "";
    if $n.block.WHAT eq 'IRx1::SubDecl' { $pull = "->($_)"};
    'for(('~$.e($n.expr)~')'~$push~"){\n"~$.e($n.block)~$pull~"\n}"
  };
  method cb__Cond ($n) {
    my $els = '';
    if $n.default { $els = "else {\n"~$.e($n.default)~"\n}" }
    my $clauses = $.e($n.clauses);
    my $first = $clauses.shift;
    my $first_test = $first[0];
    if $n.invert_first_test { $first_test = "not("~$first_test~")" }
    ('if('~$first_test~") {\n"~$first[1]~"\n}"
    ~$clauses.map(sub($e){'elsif('~$e[0]~") {\n"~$e[1]~"\n}"}).join("")
    ~$els)
  };
  method cb__Loop ($n) {
    'while('~$.e($n.pretest)~") {\n"~$.e($n.block)~"\n}"
  };

  method encode_varname($s,$t,$dsn) {
    #XXX $pkg::x -> s_pkg::x :(
    my $env = '';
    my $pre = '';
    if $t eq '+' { $env = 'e' };
    if $s eq '$' && $env eq 'e' { $pre = 's_' };
    if $s eq '@' { $pre = 'a_' }
    if $s eq '%' { $pre = 'h_' }
    my $name = $env~$pre~$dsn;
    if ($t eq '.') {
      '$self->'~$name
    } elsif ($t eq '+') {
      $name.re_gsub('::','__');
      '$'~'::'~$name
    } elsif ($t eq '*') {
      $name.re_gsub('::','__');
      '$'~'GLOBAL::'~$name
    } else {
      ''~$name
    }
  };

  method cb__Var ($n) {
    my $s = $n.sigil;
    my $t = $n.twigil||'';
    if $n.is_context { $t = '+' }
    my $dsn = $.e($n.name);
    my $v = $s~$t~$dsn;
    if $v eq '$?PACKAGE' || $v eq '$?MODULE' || $v eq '$?CLASS' {
      my $pkgname = $+whiteboard::in_package.join('::'); # XXX should use $n notes instead.
      $pkgname = $pkgname || 'Main';
      "'"~$pkgname~"'"
    } elsif $v eq '$?FILE' {
      "'"~$.filename~"'"
    } elsif $v eq '$?LINE' {
      '0' # XXX $n notes needs to provide this.
    } elsif $v eq '$?PERLVER' {
      "'elf / "~ primitive_runtime_version() ~ " / " ~ $.WHAT ~"'"
    } else {
      $.encode_varname($s,$t,$dsn);
    }
  };
  method cb__NumInt ($n) {
    $.e($n.text)
  };
  method cb__Hash ($n) {
    my $+whiteboard::emit_pairs_inline = 1;
    '{'~$.e($n.hash||[]).join(",")~'}'
  };
  method cb__Buf ($n) {
    '"' ~ quotemeta($n.buf) ~ '"';
  };
  method cb__Rx ($n) {
    my $pat = $n.pat || '';
    'qr/'~$pat~'/'
  };
  method cb__Pair($n) {
    if $+whiteboard::emit_pairs_inline {
      my $+whiteboard::emit_pairs_inline = 0;
      '('~$.e($n.key)~' => '~$.e($n.value)~')'
    } else {
      "Pair->new('key',"~$.e($n.key)~" => 'value',"~$.e($n.value)~")"
    }
  };

};

if not($*emitter0) { $*emitter0 = EmitSBCL.new}
$*emitter1 = EmitSBCL.new;
