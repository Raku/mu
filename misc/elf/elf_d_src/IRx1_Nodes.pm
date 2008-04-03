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
package IRx1 {
  class Base {
  };
  class CompUnit is Base {
    has $.match;
    has $.statements;
    has $.filename;
    
    method newp($match,$statements,$filename) { self.new('match', $match, 'statements', $statements, 'filename', $filename) };
    method callback($emitter) { $emitter.cb__CompUnit(self) };
    method node_name() { 'CompUnit' };
    method field_names() { ['statements','filename'] };
    method field_values() { [$.statements,$.filename] };
    method ir0_describe() {
      'CompUnit('~$.statements.ir0_describe~','~$.filename.ir0_describe~')'
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
  class Use is Base {
    has $.match;
    has $.kind;
    has $.module_name;
    has $.expr;
    
    method newp($match,$kind,$module_name,$expr) { self.new('match', $match, 'kind', $kind, 'module_name', $module_name, 'expr', $expr) };
    method callback($emitter) { $emitter.cb__Use(self) };
    method node_name() { 'Use' };
    method field_names() { ['kind','module_name','expr'] };
    method field_values() { [$.kind,$.module_name,$.expr] };
    method ir0_describe() {
      'Use('~$.kind.ir0_describe~','~$.module_name.ir0_describe~','~$.expr.ir0_describe~')'
    };
  };
  class PackageDecl is Base {
    has $.match;
    has $.scope;
    has $.plurality;
    has $.kind;
    has $.name;
    has $.traits;
    has $.block;
    
    method newp($match,$scope,$plurality,$kind,$name,$traits,$block) { self.new('match', $match, 'scope', $scope, 'plurality', $plurality, 'kind', $kind, 'name', $name, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__PackageDecl(self) };
    method node_name() { 'PackageDecl' };
    method field_names() { ['scope','plurality','kind','name','traits','block'] };
    method field_values() { [$.scope,$.plurality,$.kind,$.name,$.traits,$.block] };
    method ir0_describe() {
      'PackageDecl('~$.scope.ir0_describe~','~$.plurality.ir0_describe~','~$.kind.ir0_describe~','~$.name.ir0_describe~','~$.traits.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class MethodDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    has $.sigil;
    has $.postcircumfix;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block,$sigil,$postcircumfix) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block, 'sigil', $sigil, 'postcircumfix', $postcircumfix) };
    method callback($emitter) { $emitter.cb__MethodDecl(self) };
    method node_name() { 'MethodDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block','sigil','postcircumfix'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block,$.sigil,$.postcircumfix] };
    method ir0_describe() {
      'MethodDecl('~$.scope.ir0_describe~','~$.typenames.ir0_describe~','~$.plurality.ir0_describe~','~$.name.ir0_describe~','~$.multisig.ir0_describe~','~$.traits.ir0_describe~','~$.block.ir0_describe~','~$.sigil.ir0_describe~','~$.postcircumfix.ir0_describe~')'
    };
  };
  class SubDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__SubDecl(self) };
    method node_name() { 'SubDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
    method ir0_describe() {
      'SubDecl('~$.scope.ir0_describe~','~$.typenames.ir0_describe~','~$.plurality.ir0_describe~','~$.name.ir0_describe~','~$.multisig.ir0_describe~','~$.traits.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class MacroDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__MacroDecl(self) };
    method node_name() { 'MacroDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
    method ir0_describe() {
      'MacroDecl('~$.scope.ir0_describe~','~$.typenames.ir0_describe~','~$.plurality.ir0_describe~','~$.name.ir0_describe~','~$.multisig.ir0_describe~','~$.traits.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class VarDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.var;
    has $.postcircumfix;
    has $.traits;
    has $.default_op;
    has $.default_expr;
    
    method newp($match,$scope,$typenames,$plurality,$var,$postcircumfix,$traits,$default_op,$default_expr) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'var', $var, 'postcircumfix', $postcircumfix, 'traits', $traits, 'default_op', $default_op, 'default_expr', $default_expr) };
    method callback($emitter) { $emitter.cb__VarDecl(self) };
    method node_name() { 'VarDecl' };
    method field_names() { ['scope','typenames','plurality','var','postcircumfix','traits','default_op','default_expr'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.var,$.postcircumfix,$.traits,$.default_op,$.default_expr] };
    method ir0_describe() {
      'VarDecl('~$.scope.ir0_describe~','~$.typenames.ir0_describe~','~$.plurality.ir0_describe~','~$.var.ir0_describe~','~$.postcircumfix.ir0_describe~','~$.traits.ir0_describe~','~$.default_op.ir0_describe~','~$.default_expr.ir0_describe~')'
    };
  };
  class Var is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter) { $emitter.cb__Var(self) };
    method node_name() { 'Var' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
    method ir0_describe() {
      'Var('~$.sigil.ir0_describe~','~$.twigil.ir0_describe~','~$.name.ir0_describe~')'
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
  class ClosureTrait is Base {
    has $.match;
    has $.kind;
    has $.block;
    
    method newp($match,$kind,$block) { self.new('match', $match, 'kind', $kind, 'block', $block) };
    method callback($emitter) { $emitter.cb__ClosureTrait(self) };
    method node_name() { 'ClosureTrait' };
    method field_names() { ['kind','block'] };
    method field_values() { [$.kind,$.block] };
    method ir0_describe() {
      'ClosureTrait('~$.kind.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class ModuleName is Base {
    has $.match;
    has $.name;
    has $.pairs;
    
    method newp($match,$name,$pairs) { self.new('match', $match, 'name', $name, 'pairs', $pairs) };
    method callback($emitter) { $emitter.cb__ModuleName(self) };
    method node_name() { 'ModuleName' };
    method field_names() { ['name','pairs'] };
    method field_values() { [$.name,$.pairs] };
    method ir0_describe() {
      'ModuleName('~$.name.ir0_describe~','~$.pairs.ir0_describe~')'
    };
  };
  class PathName is Base {
    has $.match;
    has $.path;
    
    method newp($match,$path) { self.new('match', $match, 'path', $path) };
    method callback($emitter) { $emitter.cb__PathName(self) };
    method node_name() { 'PathName' };
    method field_names() { ['path'] };
    method field_values() { [$.path] };
    method ir0_describe() {
      'PathName('~$.path.ir0_describe~')'
    };
  };
  class SubName is Base {
    has $.match;
    has $.category;
    has $.pairs;
    has $.desigilname;
    has $.signature;
    
    method newp($match,$category,$pairs,$desigilname,$signature) { self.new('match', $match, 'category', $category, 'pairs', $pairs, 'desigilname', $desigilname, 'signature', $signature) };
    method callback($emitter) { $emitter.cb__SubName(self) };
    method node_name() { 'SubName' };
    method field_names() { ['category','pairs','desigilname','signature'] };
    method field_values() { [$.category,$.pairs,$.desigilname,$.signature] };
    method ir0_describe() {
      'SubName('~$.category.ir0_describe~','~$.pairs.ir0_describe~','~$.desigilname.ir0_describe~','~$.signature.ir0_describe~')'
    };
  };
  class ShapedParamName is Base {
    has $.match;
    has $.ident;
    has $.postcircumfix;
    
    method newp($match,$ident,$postcircumfix) { self.new('match', $match, 'ident', $ident, 'postcircumfix', $postcircumfix) };
    method callback($emitter) { $emitter.cb__ShapedParamName(self) };
    method node_name() { 'ShapedParamName' };
    method field_names() { ['ident','postcircumfix'] };
    method field_values() { [$.ident,$.postcircumfix] };
    method ir0_describe() {
      'ShapedParamName('~$.ident.ir0_describe~','~$.postcircumfix.ir0_describe~')'
    };
  };
  class Call is Base {
    has $.match;
    has $.invocant;
    has $.method;
    has $.capture;
    
    method newp($match,$invocant,$method,$capture) { self.new('match', $match, 'invocant', $invocant, 'method', $method, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Call(self) };
    method node_name() { 'Call' };
    method field_names() { ['invocant','method','capture'] };
    method field_values() { [$.invocant,$.method,$.capture] };
    method ir0_describe() {
      'Call('~$.invocant.ir0_describe~','~$.method.ir0_describe~','~$.capture.ir0_describe~')'
    };
  };
  class Apply is Base {
    has $.match;
    has $.function;
    has $.capture;
    
    method newp($match,$function,$capture) { self.new('match', $match, 'function', $function, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Apply(self) };
    method node_name() { 'Apply' };
    method field_names() { ['function','capture'] };
    method field_values() { [$.function,$.capture] };
    method ir0_describe() {
      'Apply('~$.function.ir0_describe~','~$.capture.ir0_describe~')'
    };
  };
  class Hyper is Base {
    has $.match;
    has $.operator;
    has $.capture;
    
    method newp($match,$operator,$capture) { self.new('match', $match, 'operator', $operator, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Hyper(self) };
    method node_name() { 'Hyper' };
    method field_names() { ['operator','capture'] };
    method field_values() { [$.operator,$.capture] };
    method ir0_describe() {
      'Hyper('~$.operator.ir0_describe~','~$.capture.ir0_describe~')'
    };
  };
  class Capture is Base {
    has $.match;
    has $.arguments;
    
    method newp($match,$arguments) { self.new('match', $match, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Capture(self) };
    method node_name() { 'Capture' };
    method field_names() { ['arguments'] };
    method field_values() { [$.arguments] };
    method ir0_describe() {
      'Capture('~$.arguments.ir0_describe~')'
    };
  };
  class MultiSig is Base {
    has $.match;
    has $.signatures;
    
    method newp($match,$signatures) { self.new('match', $match, 'signatures', $signatures) };
    method callback($emitter) { $emitter.cb__MultiSig(self) };
    method node_name() { 'MultiSig' };
    method field_names() { ['signatures'] };
    method field_values() { [$.signatures] };
    method ir0_describe() {
      'MultiSig('~$.signatures.ir0_describe~')'
    };
  };
  class Signature is Base {
    has $.match;
    has $.parameters;
    has $.return_type;
    
    method newp($match,$parameters,$return_type) { self.new('match', $match, 'parameters', $parameters, 'return_type', $return_type) };
    method callback($emitter) { $emitter.cb__Signature(self) };
    method node_name() { 'Signature' };
    method field_names() { ['parameters','return_type'] };
    method field_values() { [$.parameters,$.return_type] };
    method ir0_describe() {
      'Signature('~$.parameters.ir0_describe~','~$.return_type.ir0_describe~')'
    };
  };
  class Parameter is Base {
    has $.match;
    has $.type_constraints;
    has $.quant;
    has $.ident;
    has $.param_var;
    has $.traits;
    has $.post_constraints;
    has $.default_expr;
    
    method newp($match,$type_constraints,$quant,$ident,$param_var,$traits,$post_constraints,$default_expr) { self.new('match', $match, 'type_constraints', $type_constraints, 'quant', $quant, 'ident', $ident, 'param_var', $param_var, 'traits', $traits, 'post_constraints', $post_constraints, 'default_expr', $default_expr) };
    method callback($emitter) { $emitter.cb__Parameter(self) };
    method node_name() { 'Parameter' };
    method field_names() { ['type_constraints','quant','ident','param_var','traits','post_constraints','default_expr'] };
    method field_values() { [$.type_constraints,$.quant,$.ident,$.param_var,$.traits,$.post_constraints,$.default_expr] };
    method ir0_describe() {
      'Parameter('~$.type_constraints.ir0_describe~','~$.quant.ir0_describe~','~$.ident.ir0_describe~','~$.param_var.ir0_describe~','~$.traits.ir0_describe~','~$.post_constraints.ir0_describe~','~$.default_expr.ir0_describe~')'
    };
  };
  class TypeConstraint is Base {
    has $.match;
    has $.value;
    has $.where_expr;
    
    method newp($match,$value,$where_expr) { self.new('match', $match, 'value', $value, 'where_expr', $where_expr) };
    method callback($emitter) { $emitter.cb__TypeConstraint(self) };
    method node_name() { 'TypeConstraint' };
    method field_names() { ['value','where_expr'] };
    method field_values() { [$.value,$.where_expr] };
    method ir0_describe() {
      'TypeConstraint('~$.value.ir0_describe~','~$.where_expr.ir0_describe~')'
    };
  };
  class PostConstraint is Base {
    has $.match;
    has $.multisig;
    has $.where_expr;
    
    method newp($match,$multisig,$where_expr) { self.new('match', $match, 'multisig', $multisig, 'where_expr', $where_expr) };
    method callback($emitter) { $emitter.cb__PostConstraint(self) };
    method node_name() { 'PostConstraint' };
    method field_names() { ['multisig','where_expr'] };
    method field_values() { [$.multisig,$.where_expr] };
    method ir0_describe() {
      'PostConstraint('~$.multisig.ir0_describe~','~$.where_expr.ir0_describe~')'
    };
  };
  class ParamVar is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter) { $emitter.cb__ParamVar(self) };
    method node_name() { 'ParamVar' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
    method ir0_describe() {
      'ParamVar('~$.sigil.ir0_describe~','~$.twigil.ir0_describe~','~$.name.ir0_describe~')'
    };
  };
  class Undef is Base {
    has $.match;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter) { $emitter.cb__Undef(self) };
    method node_name() { 'Undef' };
    method field_names() { [] };
    method field_values() { [] };
    method ir0_describe() {
      'Undef('~')'
    };
  };
  class NumInt is Base {
    has $.match;
    has $.text;
    has $.base;
    
    method newp($match,$text,$base) { self.new('match', $match, 'text', $text, 'base', $base) };
    method callback($emitter) { $emitter.cb__NumInt(self) };
    method node_name() { 'NumInt' };
    method field_names() { ['text','base'] };
    method field_values() { [$.text,$.base] };
    method ir0_describe() {
      'NumInt('~$.text.ir0_describe~','~$.base.ir0_describe~')'
    };
  };
  class NumDec is Base {
    has $.match;
    has $.intpart;
    has $.fracpart;
    has $.exp;
    
    method newp($match,$intpart,$fracpart,$exp) { self.new('match', $match, 'intpart', $intpart, 'fracpart', $fracpart, 'exp', $exp) };
    method callback($emitter) { $emitter.cb__NumDec(self) };
    method node_name() { 'NumDec' };
    method field_names() { ['intpart','fracpart','exp'] };
    method field_values() { [$.intpart,$.fracpart,$.exp] };
    method ir0_describe() {
      'NumDec('~$.intpart.ir0_describe~','~$.fracpart.ir0_describe~','~$.exp.ir0_describe~')'
    };
  };
  class NumRad is Base {
    has $.match;
    has $.radix;
    has $.intpart;
    has $.fracpart;
    has $.base;
    has $.exp;
    
    method newp($match,$radix,$intpart,$fracpart,$base,$exp) { self.new('match', $match, 'radix', $radix, 'intpart', $intpart, 'fracpart', $fracpart, 'base', $base, 'exp', $exp) };
    method callback($emitter) { $emitter.cb__NumRad(self) };
    method node_name() { 'NumRad' };
    method field_names() { ['radix','intpart','fracpart','base','exp'] };
    method field_values() { [$.radix,$.intpart,$.fracpart,$.base,$.exp] };
    method ir0_describe() {
      'NumRad('~$.radix.ir0_describe~','~$.intpart.ir0_describe~','~$.fracpart.ir0_describe~','~$.base.ir0_describe~','~$.exp.ir0_describe~')'
    };
  };
  class Array is Base {
    has $.match;
    has $.array;
    
    method newp($match,$array) { self.new('match', $match, 'array', $array) };
    method callback($emitter) { $emitter.cb__Array(self) };
    method node_name() { 'Array' };
    method field_names() { ['array'] };
    method field_values() { [$.array] };
    method ir0_describe() {
      'Array('~$.array.ir0_describe~')'
    };
  };
  class Hash is Base {
    has $.match;
    has $.hash;
    
    method newp($match,$hash) { self.new('match', $match, 'hash', $hash) };
    method callback($emitter) { $emitter.cb__Hash(self) };
    method node_name() { 'Hash' };
    method field_names() { ['hash'] };
    method field_values() { [$.hash] };
    method ir0_describe() {
      'Hash('~$.hash.ir0_describe~')'
    };
  };
  class Pair is Base {
    has $.match;
    has $.key;
    has $.value;
    
    method newp($match,$key,$value) { self.new('match', $match, 'key', $key, 'value', $value) };
    method callback($emitter) { $emitter.cb__Pair(self) };
    method node_name() { 'Pair' };
    method field_names() { ['key','value'] };
    method field_values() { [$.key,$.value] };
    method ir0_describe() {
      'Pair('~$.key.ir0_describe~','~$.value.ir0_describe~')'
    };
  };
  class Type is Base {
    has $.match;
    has $.typename;
    
    method newp($match,$typename) { self.new('match', $match, 'typename', $typename) };
    method callback($emitter) { $emitter.cb__Type(self) };
    method node_name() { 'Type' };
    method field_names() { ['typename'] };
    method field_values() { [$.typename] };
    method ir0_describe() {
      'Type('~$.typename.ir0_describe~')'
    };
  };
  class Rx is Base {
    has $.match;
    has $.pat;
    
    method newp($match,$pat) { self.new('match', $match, 'pat', $pat) };
    method callback($emitter) { $emitter.cb__Rx(self) };
    method node_name() { 'Rx' };
    method field_names() { ['pat'] };
    method field_values() { [$.pat] };
    method ir0_describe() {
      'Rx('~$.pat.ir0_describe~')'
    };
  };
  class Buf is Base {
    has $.match;
    has $.buf;
    
    method newp($match,$buf) { self.new('match', $match, 'buf', $buf) };
    method callback($emitter) { $emitter.cb__Buf(self) };
    method node_name() { 'Buf' };
    method field_names() { ['buf'] };
    method field_values() { [$.buf] };
    method ir0_describe() {
      'Buf('~$.buf.ir0_describe~')'
    };
  };
  class For is Base {
    has $.match;
    has $.expr;
    has $.block;
    
    method newp($match,$expr,$block) { self.new('match', $match, 'expr', $expr, 'block', $block) };
    method callback($emitter) { $emitter.cb__For(self) };
    method node_name() { 'For' };
    method field_names() { ['expr','block'] };
    method field_values() { [$.expr,$.block] };
    method ir0_describe() {
      'For('~$.expr.ir0_describe~','~$.block.ir0_describe~')'
    };
  };
  class Cond is Base {
    has $.match;
    has $.clauses;
    has $.default;
    has $.invert_first_test;
    
    method newp($match,$clauses,$default,$invert_first_test) { self.new('match', $match, 'clauses', $clauses, 'default', $default, 'invert_first_test', $invert_first_test) };
    method callback($emitter) { $emitter.cb__Cond(self) };
    method node_name() { 'Cond' };
    method field_names() { ['clauses','default','invert_first_test'] };
    method field_values() { [$.clauses,$.default,$.invert_first_test] };
    method ir0_describe() {
      'Cond('~$.clauses.ir0_describe~','~$.default.ir0_describe~','~$.invert_first_test.ir0_describe~')'
    };
  };
  class Loop is Base {
    has $.match;
    has $.pretest;
    has $.block;
    has $.posttest;
    has $.label;
    
    method newp($match,$pretest,$block,$posttest,$label) { self.new('match', $match, 'pretest', $pretest, 'block', $block, 'posttest', $posttest, 'label', $label) };
    method callback($emitter) { $emitter.cb__Loop(self) };
    method node_name() { 'Loop' };
    method field_names() { ['pretest','block','posttest','label'] };
    method field_values() { [$.pretest,$.block,$.posttest,$.label] };
    method ir0_describe() {
      'Loop('~$.pretest.ir0_describe~','~$.block.ir0_describe~','~$.posttest.ir0_describe~','~$.label.ir0_describe~')'
    };
  };
}
