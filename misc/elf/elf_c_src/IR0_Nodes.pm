#line 2 ir_nodes.p6
# Warning: This file is mechanically written.  Your changes will be overwritten.
package ARRAY {
  method ir0_describe() {
    '[' ~ self.map(sub($e){$e.ir0_describe}).join(",") ~ ']'
  };
};
package SCALAR {
  method ir0_describe() {
    self ~ ""
  };
};
package UNDEF {
  method ir0_describe() {
    'undef'
  };
};
package IR0 {
  class Base {
  };
  class Val_Base is Base {
  };
  class Lit_Base is Base {
  };
  class Rule_Base is Base {
  };
  class CompUnit is Base {
    has $.match;
    has $.statements;
    
    method newp($match,$statements) { self.new('match', $match, 'statements', $statements) };
    method callback($emitter) { $emitter.cb__CompUnit(self) };
    method node_name() { 'CompUnit' };
    method field_names() { ['statements'] };
    method field_values() { [$.statements] };
    method ir0_describe() {
      'CompUnit('~$.statements.ir0_describe~')'
    };
  };
  class Val_Int is Val_Base {
    has $.match;
    has $.text;
    
    method newp($match,$text) { self.new('match', $match, 'text', $text) };
    method callback($emitter) { $emitter.cb__Val_Int(self) };
    method node_name() { 'Val_Int' };
    method field_names() { ['text'] };
    method field_values() { [$.text] };
    method ir0_describe() {
      'Val_Int('~$.text.ir0_describe~')'
    };
  };
  class PackageDeclarator is Base {
    has $.match;
    has $.kind;
    has $.name;
    has $.traits;
    has $.block;
    
    method newp($match,$kind,$name,$traits,$block) { self.new('match', $match, 'kind', $kind, 'name', $name, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__PackageDeclarator(self) };
    method node_name() { 'PackageDeclarator' };
    method field_names() { ['kind','name','traits','block'] };
    method field_values() { [$.kind,$.name,$.traits,$.block] };
    method ir0_describe() {
      'PackageDeclarator('~$.kind.ir0_describe~','~$.name.ir0_describe~','~$.traits.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Trait is Base {
    has $.match;
    has $.verb;
    has $.expr;
    
    method newp($match,$verb,$expr) { self.new('match', $match, 'verb', $verb, 'expr', $expr) };
    method callback($emitter) { $emitter.cb__Trait(self) };
    method node_name() { 'Trait' };
    method field_names() { ['verb','expr'] };
    method field_values() { [$.verb,$.expr] };
    method ir0_describe() {
      'Trait('~$.verb.ir0_describe~','~$.expr.ir0_describe~')'
    };
  };
  class Block is Base {
    has $.match;
    has $.statements;
    
    method newp($match,$statements) { self.new('match', $match, 'statements', $statements) };
    method callback($emitter) { $emitter.cb__Block(self) };
    method node_name() { 'Block' };
    method field_names() { ['statements'] };
    method field_values() { [$.statements] };
    method ir0_describe() {
      'Block('~$.statements.ir0_describe~')'
    };
  };
  class Quote is Base {
    has $.match;
    has $.concat;
    
    method newp($match,$concat) { self.new('match', $match, 'concat', $concat) };
    method callback($emitter) { $emitter.cb__Quote(self) };
    method node_name() { 'Quote' };
    method field_names() { ['concat'] };
    method field_values() { [$.concat] };
    method ir0_describe() {
      'Quote('~$.concat.ir0_describe~')'
    };
  };
  class Val_Bit is Val_Base {
    has $.match;
    has $.bit;
    
    method newp($match,$bit) { self.new('match', $match, 'bit', $bit) };
    method callback($emitter) { $emitter.cb__Val_Bit(self) };
    method node_name() { 'Val_Bit' };
    method field_names() { ['bit'] };
    method field_values() { [$.bit] };
    method ir0_describe() {
      'Val_Bit('~$.bit.ir0_describe~')'
    };
  };
  class Val_Num is Val_Base {
    has $.match;
    has $.num;
    
    method newp($match,$num) { self.new('match', $match, 'num', $num) };
    method callback($emitter) { $emitter.cb__Val_Num(self) };
    method node_name() { 'Val_Num' };
    method field_names() { ['num'] };
    method field_values() { [$.num] };
    method ir0_describe() {
      'Val_Num('~$.num.ir0_describe~')'
    };
  };
  class Val_Buf is Val_Base {
    has $.match;
    has $.buf;
    
    method newp($match,$buf) { self.new('match', $match, 'buf', $buf) };
    method callback($emitter) { $emitter.cb__Val_Buf(self) };
    method node_name() { 'Val_Buf' };
    method field_names() { ['buf'] };
    method field_values() { [$.buf] };
    method ir0_describe() {
      'Val_Buf('~$.buf.ir0_describe~')'
    };
  };
  class Val_Char is Val_Base {
    has $.match;
    has $.char;
    
    method newp($match,$char) { self.new('match', $match, 'char', $char) };
    method callback($emitter) { $emitter.cb__Val_Char(self) };
    method node_name() { 'Val_Char' };
    method field_names() { ['char'] };
    method field_values() { [$.char] };
    method ir0_describe() {
      'Val_Char('~$.char.ir0_describe~')'
    };
  };
  class Val_Undef is Val_Base {
    has $.match;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter) { $emitter.cb__Val_Undef(self) };
    method node_name() { 'Val_Undef' };
    method field_names() { [] };
    method field_values() { [] };
    method ir0_describe() {
      'Val_Undef('~')'
    };
  };
  class Val_Object is Val_Base {
    has $.match;
    has $.clazz;
    has $.fields;
    
    method newp($match,$clazz,$fields) { self.new('match', $match, 'clazz', $clazz, 'fields', $fields) };
    method callback($emitter) { $emitter.cb__Val_Object(self) };
    method node_name() { 'Val_Object' };
    method field_names() { ['clazz','fields'] };
    method field_values() { [$.clazz,$.fields] };
    method ir0_describe() {
      'Val_Object('~$.clazz.ir0_describe~','~$.fields.ir0_describe~')'
    };
  };
  class Val_Rx is Val_Base {
    has $.match;
    has $.pat;
    
    method newp($match,$pat) { self.new('match', $match, 'pat', $pat) };
    method callback($emitter) { $emitter.cb__Val_Rx(self) };
    method node_name() { 'Val_Rx' };
    method field_names() { ['pat'] };
    method field_values() { [$.pat] };
    method ir0_describe() {
      'Val_Rx('~$.pat.ir0_describe~')'
    };
  };
  class Lit_Seq is Lit_Base {
    has $.match;
    has $.seq;
    
    method newp($match,$seq) { self.new('match', $match, 'seq', $seq) };
    method callback($emitter) { $emitter.cb__Lit_Seq(self) };
    method node_name() { 'Lit_Seq' };
    method field_names() { ['seq'] };
    method field_values() { [$.seq] };
    method ir0_describe() {
      'Lit_Seq('~$.seq.ir0_describe~')'
    };
  };
  class Lit_Array is Lit_Base {
    has $.match;
    has $.array;
    
    method newp($match,$array) { self.new('match', $match, 'array', $array) };
    method callback($emitter) { $emitter.cb__Lit_Array(self) };
    method node_name() { 'Lit_Array' };
    method field_names() { ['array'] };
    method field_values() { [$.array] };
    method ir0_describe() {
      'Lit_Array('~$.array.ir0_describe~')'
    };
  };
  class Lit_Hash is Lit_Base {
    has $.match;
    has $.hash;
    
    method newp($match,$hash) { self.new('match', $match, 'hash', $hash) };
    method callback($emitter) { $emitter.cb__Lit_Hash(self) };
    method node_name() { 'Lit_Hash' };
    method field_names() { ['hash'] };
    method field_values() { [$.hash] };
    method ir0_describe() {
      'Lit_Hash('~$.hash.ir0_describe~')'
    };
  };
  class Lit_Pair is Lit_Base {
    has $.match;
    has $.key;
    has $.value;
    
    method newp($match,$key,$value) { self.new('match', $match, 'key', $key, 'value', $value) };
    method callback($emitter) { $emitter.cb__Lit_Pair(self) };
    method node_name() { 'Lit_Pair' };
    method field_names() { ['key','value'] };
    method field_values() { [$.key,$.value] };
    method ir0_describe() {
      'Lit_Pair('~$.key.ir0_describe~','~$.value.ir0_describe~')'
    };
  };
  class Lit_SigArgument is Lit_Base {
    has $.match;
    has $.key;
    has $.value;
    has $.type;
    has $.has_default;
    has $.is_named_only;
    has $.is_optional;
    has $.is_slurpy;
    has $.is_multidimensional;
    has $.is_rw;
    has $.is_copy;
    
    method newp($match,$key,$value,$type,$has_default,$is_named_only,$is_optional,$is_slurpy,$is_multidimensional,$is_rw,$is_copy) { self.new('match', $match, 'key', $key, 'value', $value, 'type', $type, 'has_default', $has_default, 'is_named_only', $is_named_only, 'is_optional', $is_optional, 'is_slurpy', $is_slurpy, 'is_multidimensional', $is_multidimensional, 'is_rw', $is_rw, 'is_copy', $is_copy) };
    method callback($emitter) { $emitter.cb__Lit_SigArgument(self) };
    method node_name() { 'Lit_SigArgument' };
    method field_names() { ['key','value','type','has_default','is_named_only','is_optional','is_slurpy','is_multidimensional','is_rw','is_copy'] };
    method field_values() { [$.key,$.value,$.type,$.has_default,$.is_named_only,$.is_optional,$.is_slurpy,$.is_multidimensional,$.is_rw,$.is_copy] };
    method ir0_describe() {
      'Lit_SigArgument('~$.key.ir0_describe~','~$.value.ir0_describe~','~$.type.ir0_describe~','~$.has_default.ir0_describe~','~$.is_named_only.ir0_describe~','~$.is_optional.ir0_describe~','~$.is_slurpy.ir0_describe~','~$.is_multidimensional.ir0_describe~','~$.is_rw.ir0_describe~','~$.is_copy.ir0_describe~')'
    };
  };
  class Lit_NamedArgument is Lit_Base {
    has $.match;
    has $.key;
    has $.value;
    
    method newp($match,$key,$value) { self.new('match', $match, 'key', $key, 'value', $value) };
    method callback($emitter) { $emitter.cb__Lit_NamedArgument(self) };
    method node_name() { 'Lit_NamedArgument' };
    method field_names() { ['key','value'] };
    method field_values() { [$.key,$.value] };
    method ir0_describe() {
      'Lit_NamedArgument('~$.key.ir0_describe~','~$.value.ir0_describe~')'
    };
  };
  class Lit_Code is Lit_Base {
    has $.match;
    has $.pad;
    has $.state;
    has $.sig;
    has $.body;
    has $.catch;
    
    method newp($match,$pad,$state,$sig,$body,$catch) { self.new('match', $match, 'pad', $pad, 'state', $state, 'sig', $sig, 'body', $body, 'catch', $catch) };
    method callback($emitter) { $emitter.cb__Lit_Code(self) };
    method node_name() { 'Lit_Code' };
    method field_names() { ['pad','state','sig','body','catch'] };
    method field_values() { [$.pad,$.state,$.sig,$.body,$.catch] };
    method ir0_describe() {
      'Lit_Code('~$.pad.ir0_describe~','~$.state.ir0_describe~','~$.sig.ir0_describe~','~$.body.ir0_describe~','~$.catch.ir0_describe~')'
    };
  };
  class Lit_Object is Lit_Base {
    has $.match;
    has $.clazz;
    has $.fields;
    
    method newp($match,$clazz,$fields) { self.new('match', $match, 'clazz', $clazz, 'fields', $fields) };
    method callback($emitter) { $emitter.cb__Lit_Object(self) };
    method node_name() { 'Lit_Object' };
    method field_names() { ['clazz','fields'] };
    method field_values() { [$.clazz,$.fields] };
    method ir0_describe() {
      'Lit_Object('~$.clazz.ir0_describe~','~$.fields.ir0_describe~')'
    };
  };
  class Var is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.namespace;
    
    method newp($match,$sigil,$twigil,$name,$namespace) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name, 'namespace', $namespace) };
    method callback($emitter) { $emitter.cb__Var(self) };
    method node_name() { 'Var' };
    method field_names() { ['sigil','twigil','name','namespace'] };
    method field_values() { [$.sigil,$.twigil,$.name,$.namespace] };
    method ir0_describe() {
      'Var('~$.sigil.ir0_describe~','~$.twigil.ir0_describe~','~$.name.ir0_describe~','~$.namespace.ir0_describe~')'
    };
  };
  class Bind is Base {
    has $.match;
    has $.parameters;
    has $.arguments;
    
    method newp($match,$parameters,$arguments) { self.new('match', $match, 'parameters', $parameters, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Bind(self) };
    method node_name() { 'Bind' };
    method field_names() { ['parameters','arguments'] };
    method field_values() { [$.parameters,$.arguments] };
    method ir0_describe() {
      'Bind('~$.parameters.ir0_describe~','~$.arguments.ir0_describe~')'
    };
  };
  class Assign is Base {
    has $.match;
    has $.parameters;
    has $.arguments;
    
    method newp($match,$parameters,$arguments) { self.new('match', $match, 'parameters', $parameters, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Assign(self) };
    method node_name() { 'Assign' };
    method field_names() { ['parameters','arguments'] };
    method field_values() { [$.parameters,$.arguments] };
    method ir0_describe() {
      'Assign('~$.parameters.ir0_describe~','~$.arguments.ir0_describe~')'
    };
  };
  class Proto is Base {
    has $.match;
    has $.name;
    
    method newp($match,$name) { self.new('match', $match, 'name', $name) };
    method callback($emitter) { $emitter.cb__Proto(self) };
    method node_name() { 'Proto' };
    method field_names() { ['name'] };
    method field_values() { [$.name] };
    method ir0_describe() {
      'Proto('~$.name.ir0_describe~')'
    };
  };
  class Call is Base {
    has $.match;
    has $.invocant;
    has $.hyper;
    has $.method;
    has $.arguments;
    
    method newp($match,$invocant,$hyper,$method,$arguments) { self.new('match', $match, 'invocant', $invocant, 'hyper', $hyper, 'method', $method, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Call(self) };
    method node_name() { 'Call' };
    method field_names() { ['invocant','hyper','method','arguments'] };
    method field_values() { [$.invocant,$.hyper,$.method,$.arguments] };
    method ir0_describe() {
      'Call('~$.invocant.ir0_describe~','~$.hyper.ir0_describe~','~$.method.ir0_describe~','~$.arguments.ir0_describe~')'
    };
  };
  class Apply is Base {
    has $.match;
    has $.code;
    has $.arguments;
    
    method newp($match,$code,$arguments) { self.new('match', $match, 'code', $code, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Apply(self) };
    method node_name() { 'Apply' };
    method field_names() { ['code','arguments'] };
    method field_values() { [$.code,$.arguments] };
    method ir0_describe() {
      'Apply('~$.code.ir0_describe~','~$.arguments.ir0_describe~')'
    };
  };
  class Return is Base {
    has $.match;
    has $.result;
    
    method newp($match,$result) { self.new('match', $match, 'result', $result) };
    method callback($emitter) { $emitter.cb__Return(self) };
    method node_name() { 'Return' };
    method field_names() { ['result'] };
    method field_values() { [$.result] };
    method ir0_describe() {
      'Return('~$.result.ir0_describe~')'
    };
  };
  class If is Base {
    has $.match;
    has $.test;
    has $.body;
    has $.elsif;
    has $.else;
    
    method newp($match,$test,$body,$elsif,$else) { self.new('match', $match, 'test', $test, 'body', $body, 'elsif', $elsif, 'else', $else) };
    method callback($emitter) { $emitter.cb__If(self) };
    method node_name() { 'If' };
    method field_names() { ['test','body','elsif','else'] };
    method field_values() { [$.test,$.body,$.elsif,$.else] };
    method ir0_describe() {
      'If('~$.test.ir0_describe~','~$.body.ir0_describe~','~$.elsif.ir0_describe~','~$.else.ir0_describe~')'
    };
  };
  class While is Base {
    has $.match;
    has $.test;
    has $.body;
    
    method newp($match,$test,$body) { self.new('match', $match, 'test', $test, 'body', $body) };
    method callback($emitter) { $emitter.cb__While(self) };
    method node_name() { 'While' };
    method field_names() { ['test','body'] };
    method field_values() { [$.test,$.body] };
    method ir0_describe() {
      'While('~$.test.ir0_describe~','~$.body.ir0_describe~')'
    };
  };
  class For is Base {
    has $.match;
    has $.expr;
    has $.body;
    
    method newp($match,$expr,$body) { self.new('match', $match, 'expr', $expr, 'body', $body) };
    method callback($emitter) { $emitter.cb__For(self) };
    method node_name() { 'For' };
    method field_names() { ['expr','body'] };
    method field_values() { [$.expr,$.body] };
    method ir0_describe() {
      'For('~$.expr.ir0_describe~','~$.body.ir0_describe~')'
    };
  };
  class Decl is Base {
    has $.match;
    has $.decl;
    has $.type;
    has $.var;
    has $.default;
    
    method newp($match,$decl,$type,$var,$default) { self.new('match', $match, 'decl', $decl, 'type', $type, 'var', $var, 'default', $default) };
    method callback($emitter) { $emitter.cb__Decl(self) };
    method node_name() { 'Decl' };
    method field_names() { ['decl','type','var','default'] };
    method field_values() { [$.decl,$.type,$.var,$.default] };
    method ir0_describe() {
      'Decl('~$.decl.ir0_describe~','~$.type.ir0_describe~','~$.var.ir0_describe~','~$.default.ir0_describe~')'
    };
  };
  class Sig is Base {
    has $.match;
    has $.invocant;
    has $.positional;
    
    method newp($match,$invocant,$positional) { self.new('match', $match, 'invocant', $invocant, 'positional', $positional) };
    method callback($emitter) { $emitter.cb__Sig(self) };
    method node_name() { 'Sig' };
    method field_names() { ['invocant','positional'] };
    method field_values() { [$.invocant,$.positional] };
    method ir0_describe() {
      'Sig('~$.invocant.ir0_describe~','~$.positional.ir0_describe~')'
    };
  };
  class Lit_Capture is Lit_Base {
    has $.match;
    has $.invocant;
    has $.array;
    has $.hash;
    
    method newp($match,$invocant,$array,$hash) { self.new('match', $match, 'invocant', $invocant, 'array', $array, 'hash', $hash) };
    method callback($emitter) { $emitter.cb__Lit_Capture(self) };
    method node_name() { 'Lit_Capture' };
    method field_names() { ['invocant','array','hash'] };
    method field_values() { [$.invocant,$.array,$.hash] };
    method ir0_describe() {
      'Lit_Capture('~$.invocant.ir0_describe~','~$.array.ir0_describe~','~$.hash.ir0_describe~')'
    };
  };
  class Lit_Subset is Lit_Base {
    has $.match;
    has $.name;
    has $.base_class;
    has $.block;
    
    method newp($match,$name,$base_class,$block) { self.new('match', $match, 'name', $name, 'base_class', $base_class, 'block', $block) };
    method callback($emitter) { $emitter.cb__Lit_Subset(self) };
    method node_name() { 'Lit_Subset' };
    method field_names() { ['name','base_class','block'] };
    method field_values() { [$.name,$.base_class,$.block] };
    method ir0_describe() {
      'Lit_Subset('~$.name.ir0_describe~','~$.base_class.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Method is Base {
    has $.match;
    has $.name;
    has $.sig;
    has $.block;
    
    method newp($match,$name,$sig,$block) { self.new('match', $match, 'name', $name, 'sig', $sig, 'block', $block) };
    method callback($emitter) { $emitter.cb__Method(self) };
    method node_name() { 'Method' };
    method field_names() { ['name','sig','block'] };
    method field_values() { [$.name,$.sig,$.block] };
    method ir0_describe() {
      'Method('~$.name.ir0_describe~','~$.sig.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Sub is Base {
    has $.match;
    has $.name;
    has $.sig;
    has $.block;
    
    method newp($match,$name,$sig,$block) { self.new('match', $match, 'name', $name, 'sig', $sig, 'block', $block) };
    method callback($emitter) { $emitter.cb__Sub(self) };
    method node_name() { 'Sub' };
    method field_names() { ['name','sig','block'] };
    method field_values() { [$.name,$.sig,$.block] };
    method ir0_describe() {
      'Sub('~$.name.ir0_describe~','~$.sig.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Macro is Base {
    has $.match;
    has $.name;
    has $.sig;
    has $.block;
    
    method newp($match,$name,$sig,$block) { self.new('match', $match, 'name', $name, 'sig', $sig, 'block', $block) };
    method callback($emitter) { $emitter.cb__Macro(self) };
    method node_name() { 'Macro' };
    method field_names() { ['name','sig','block'] };
    method field_values() { [$.name,$.sig,$.block] };
    method ir0_describe() {
      'Macro('~$.name.ir0_describe~','~$.sig.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Coro is Base {
    has $.match;
    has $.name;
    has $.sig;
    has $.block;
    
    method newp($match,$name,$sig,$block) { self.new('match', $match, 'name', $name, 'sig', $sig, 'block', $block) };
    method callback($emitter) { $emitter.cb__Coro(self) };
    method node_name() { 'Coro' };
    method field_names() { ['name','sig','block'] };
    method field_values() { [$.name,$.sig,$.block] };
    method ir0_describe() {
      'Coro('~$.name.ir0_describe~','~$.sig.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class P5Token is Base {
    has $.match;
    has $.regex;
    
    method newp($match,$regex) { self.new('match', $match, 'regex', $regex) };
    method callback($emitter) { $emitter.cb__P5Token(self) };
    method node_name() { 'P5Token' };
    method field_names() { ['regex'] };
    method field_values() { [$.regex] };
    method ir0_describe() {
      'P5Token('~$.regex.ir0_describe~')'
    };
  };
  class Token is Base {
    has $.match;
    has $.name;
    has $.regex;
    has $.sym;
    
    method newp($match,$name,$regex,$sym) { self.new('match', $match, 'name', $name, 'regex', $regex, 'sym', $sym) };
    method callback($emitter) { $emitter.cb__Token(self) };
    method node_name() { 'Token' };
    method field_names() { ['name','regex','sym'] };
    method field_values() { [$.name,$.regex,$.sym] };
    method ir0_describe() {
      'Token('~$.name.ir0_describe~','~$.regex.ir0_describe~','~$.sym.ir0_describe~')'
    };
  };
  class Do is Base {
    has $.match;
    has $.block;
    
    method newp($match,$block) { self.new('match', $match, 'block', $block) };
    method callback($emitter) { $emitter.cb__Do(self) };
    method node_name() { 'Do' };
    method field_names() { ['block'] };
    method field_values() { [$.block] };
    method ir0_describe() {
      'Do('~$.block.ir0_describe~')'
    };
  };
  class Begin is Base {
    has $.match;
    has $.block;
    
    method newp($match,$block) { self.new('match', $match, 'block', $block) };
    method callback($emitter) { $emitter.cb__Begin(self) };
    method node_name() { 'Begin' };
    method field_names() { ['block'] };
    method field_values() { [$.block] };
    method ir0_describe() {
      'Begin('~$.block.ir0_describe~')'
    };
  };
  class Use is Base {
    has $.match;
    has $.mod;
    has $.perl5;
    
    method newp($match,$mod,$perl5) { self.new('match', $match, 'mod', $mod, 'perl5', $perl5) };
    method callback($emitter) { $emitter.cb__Use(self) };
    method node_name() { 'Use' };
    method field_names() { ['mod','perl5'] };
    method field_values() { [$.mod,$.perl5] };
    method ir0_describe() {
      'Use('~$.mod.ir0_describe~','~$.perl5.ir0_describe~')'
    };
  };
  class Rule is Base {
    has $.match;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter) { $emitter.cb__Rule(self) };
    method node_name() { 'Rule' };
    method field_names() { [] };
    method field_values() { [] };
    method ir0_describe() {
      'Rule('~')'
    };
  };
  class Rule_Quantifier is Rule_Base {
    has $.match;
    has $.term;
    has $.quant;
    has $.greedy;
    has $.ws1;
    has $.ws2;
    has $.ws3;
    
    method newp($match,$term,$quant,$greedy,$ws1,$ws2,$ws3) { self.new('match', $match, 'term', $term, 'quant', $quant, 'greedy', $greedy, 'ws1', $ws1, 'ws2', $ws2, 'ws3', $ws3) };
    method callback($emitter) { $emitter.cb__Rule_Quantifier(self) };
    method node_name() { 'Rule_Quantifier' };
    method field_names() { ['term','quant','greedy','ws1','ws2','ws3'] };
    method field_values() { [$.term,$.quant,$.greedy,$.ws1,$.ws2,$.ws3] };
    method ir0_describe() {
      'Rule_Quantifier('~$.term.ir0_describe~','~$.quant.ir0_describe~','~$.greedy.ir0_describe~','~$.ws1.ir0_describe~','~$.ws2.ir0_describe~','~$.ws3.ir0_describe~')'
    };
  };
  class Rule_Or is Rule_Base {
    has $.match;
    has $.terms;
    
    method newp($match,$terms) { self.new('match', $match, 'terms', $terms) };
    method callback($emitter) { $emitter.cb__Rule_Or(self) };
    method node_name() { 'Rule_Or' };
    method field_names() { ['terms'] };
    method field_values() { [$.terms] };
    method ir0_describe() {
      'Rule_Or('~$.terms.ir0_describe~')'
    };
  };
  class Rule_Concat is Rule_Base {
    has $.match;
    has $.concat;
    
    method newp($match,$concat) { self.new('match', $match, 'concat', $concat) };
    method callback($emitter) { $emitter.cb__Rule_Concat(self) };
    method node_name() { 'Rule_Concat' };
    method field_names() { ['concat'] };
    method field_values() { [$.concat] };
    method ir0_describe() {
      'Rule_Concat('~$.concat.ir0_describe~')'
    };
  };
  class Rule_Subrule is Rule_Base {
    has $.match;
    has $.metasyntax;
    has $.ident;
    has $.capture_to_array;
    
    method newp($match,$metasyntax,$ident,$capture_to_array) { self.new('match', $match, 'metasyntax', $metasyntax, 'ident', $ident, 'capture_to_array', $capture_to_array) };
    method callback($emitter) { $emitter.cb__Rule_Subrule(self) };
    method node_name() { 'Rule_Subrule' };
    method field_names() { ['metasyntax','ident','capture_to_array'] };
    method field_values() { [$.metasyntax,$.ident,$.capture_to_array] };
    method ir0_describe() {
      'Rule_Subrule('~$.metasyntax.ir0_describe~','~$.ident.ir0_describe~','~$.capture_to_array.ir0_describe~')'
    };
  };
  class Rule_SubruleNoCapture is Rule_Base {
    has $.match;
    has $.metasyntax;
    
    method newp($match,$metasyntax) { self.new('match', $match, 'metasyntax', $metasyntax) };
    method callback($emitter) { $emitter.cb__Rule_SubruleNoCapture(self) };
    method node_name() { 'Rule_SubruleNoCapture' };
    method field_names() { ['metasyntax'] };
    method field_values() { [$.metasyntax] };
    method ir0_describe() {
      'Rule_SubruleNoCapture('~$.metasyntax.ir0_describe~')'
    };
  };
  class Rule_Var is Rule_Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter) { $emitter.cb__Rule_Var(self) };
    method node_name() { 'Rule_Var' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
    method ir0_describe() {
      'Rule_Var('~$.sigil.ir0_describe~','~$.twigil.ir0_describe~','~$.name.ir0_describe~')'
    };
  };
  class Rule_Constant is Rule_Base {
    has $.match;
    has $.constant;
    
    method newp($match,$constant) { self.new('match', $match, 'constant', $constant) };
    method callback($emitter) { $emitter.cb__Rule_Constant(self) };
    method node_name() { 'Rule_Constant' };
    method field_names() { ['constant'] };
    method field_values() { [$.constant] };
    method ir0_describe() {
      'Rule_Constant('~$.constant.ir0_describe~')'
    };
  };
  class Rule_Dot is Rule_Base {
    has $.match;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter) { $emitter.cb__Rule_Dot(self) };
    method node_name() { 'Rule_Dot' };
    method field_names() { [] };
    method field_values() { [] };
    method ir0_describe() {
      'Rule_Dot('~')'
    };
  };
  class Rule_SpecialChar is Rule_Base {
    has $.match;
    has $.char;
    
    method newp($match,$char) { self.new('match', $match, 'char', $char) };
    method callback($emitter) { $emitter.cb__Rule_SpecialChar(self) };
    method node_name() { 'Rule_SpecialChar' };
    method field_names() { ['char'] };
    method field_values() { [$.char] };
    method ir0_describe() {
      'Rule_SpecialChar('~$.char.ir0_describe~')'
    };
  };
  class Rule_Block is Rule_Base {
    has $.match;
    has $.closure;
    
    method newp($match,$closure) { self.new('match', $match, 'closure', $closure) };
    method callback($emitter) { $emitter.cb__Rule_Block(self) };
    method node_name() { 'Rule_Block' };
    method field_names() { ['closure'] };
    method field_values() { [$.closure] };
    method ir0_describe() {
      'Rule_Block('~$.closure.ir0_describe~')'
    };
  };
  class Rule_InterpolateVar is Rule_Base {
    has $.match;
    has $.var;
    
    method newp($match,$var) { self.new('match', $match, 'var', $var) };
    method callback($emitter) { $emitter.cb__Rule_InterpolateVar(self) };
    method node_name() { 'Rule_InterpolateVar' };
    method field_names() { ['var'] };
    method field_values() { [$.var] };
    method ir0_describe() {
      'Rule_InterpolateVar('~$.var.ir0_describe~')'
    };
  };
  class Rule_NamedCapture is Rule_Base {
    has $.match;
    has $.rule;
    has $.ident;
    has $.capture_to_array;
    
    method newp($match,$rule,$ident,$capture_to_array) { self.new('match', $match, 'rule', $rule, 'ident', $ident, 'capture_to_array', $capture_to_array) };
    method callback($emitter) { $emitter.cb__Rule_NamedCapture(self) };
    method node_name() { 'Rule_NamedCapture' };
    method field_names() { ['rule','ident','capture_to_array'] };
    method field_values() { [$.rule,$.ident,$.capture_to_array] };
    method ir0_describe() {
      'Rule_NamedCapture('~$.rule.ir0_describe~','~$.ident.ir0_describe~','~$.capture_to_array.ir0_describe~')'
    };
  };
  class Rule_Before is Rule_Base {
    has $.match;
    has $.rule;
    has $.assertion_modifier;
    has $.capture_to_array;
    
    method newp($match,$rule,$assertion_modifier,$capture_to_array) { self.new('match', $match, 'rule', $rule, 'assertion_modifier', $assertion_modifier, 'capture_to_array', $capture_to_array) };
    method callback($emitter) { $emitter.cb__Rule_Before(self) };
    method node_name() { 'Rule_Before' };
    method field_names() { ['rule','assertion_modifier','capture_to_array'] };
    method field_values() { [$.rule,$.assertion_modifier,$.capture_to_array] };
    method ir0_describe() {
      'Rule_Before('~$.rule.ir0_describe~','~$.assertion_modifier.ir0_describe~','~$.capture_to_array.ir0_describe~')'
    };
  };
  class Rule_After is Rule_Base {
    has $.match;
    has $.rule;
    has $.assertion_modifier;
    has $.capture_to_array;
    
    method newp($match,$rule,$assertion_modifier,$capture_to_array) { self.new('match', $match, 'rule', $rule, 'assertion_modifier', $assertion_modifier, 'capture_to_array', $capture_to_array) };
    method callback($emitter) { $emitter.cb__Rule_After(self) };
    method node_name() { 'Rule_After' };
    method field_names() { ['rule','assertion_modifier','capture_to_array'] };
    method field_values() { [$.rule,$.assertion_modifier,$.capture_to_array] };
    method ir0_describe() {
      'Rule_After('~$.rule.ir0_describe~','~$.assertion_modifier.ir0_describe~','~$.capture_to_array.ir0_describe~')'
    };
  };
  class Rule_NegateCharClass is Rule_Base {
    has $.match;
    has $.chars;
    
    method newp($match,$chars) { self.new('match', $match, 'chars', $chars) };
    method callback($emitter) { $emitter.cb__Rule_NegateCharClass(self) };
    method node_name() { 'Rule_NegateCharClass' };
    method field_names() { ['chars'] };
    method field_values() { [$.chars] };
    method ir0_describe() {
      'Rule_NegateCharClass('~$.chars.ir0_describe~')'
    };
  };
  class Rule_CharClass is Rule_Base {
    has $.match;
    has $.chars;
    
    method newp($match,$chars) { self.new('match', $match, 'chars', $chars) };
    method callback($emitter) { $emitter.cb__Rule_CharClass(self) };
    method node_name() { 'Rule_CharClass' };
    method field_names() { ['chars'] };
    method field_values() { [$.chars] };
    method ir0_describe() {
      'Rule_CharClass('~$.chars.ir0_describe~')'
    };
  };
  class Rule_Capture is Rule_Base {
    has $.match;
    has $.rule;
    has $.position;
    has $.capture_to_array;
    
    method newp($match,$rule,$position,$capture_to_array) { self.new('match', $match, 'rule', $rule, 'position', $position, 'capture_to_array', $capture_to_array) };
    method callback($emitter) { $emitter.cb__Rule_Capture(self) };
    method node_name() { 'Rule_Capture' };
    method field_names() { ['rule','position','capture_to_array'] };
    method field_values() { [$.rule,$.position,$.capture_to_array] };
    method ir0_describe() {
      'Rule_Capture('~$.rule.ir0_describe~','~$.position.ir0_describe~','~$.capture_to_array.ir0_describe~')'
    };
  };
}
