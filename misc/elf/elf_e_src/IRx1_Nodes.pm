# Warning: This file is mechanically written.  Your changes will be overwritten.
package ARRAY {
  method irx1_describe() {
    '[' ~ self.map(sub($e){$e.irx1_describe}).join(",") ~ ']'
  };
};
package SCALAR {
  method irx1_describe() {
    self ~ ""
  };
};
package UNDEF {
  method irx1_describe() {
    'undef'
  };
};
package IRx1 {
  class Base {
  };
  class CompUnit_and_Block {};
  class CompUnit is CompUnit_and_Block {};
  class Block    is CompUnit_and_Block {};

  class CompUnit is Base {
    has $.match;
    has $.statements;
    has $.filename;
    has $.notes;
    
    method newp($match,$statements,$filename) { self.new('match', $match, 'statements', $statements, 'filename', $filename) };
    method callback($emitter) { $emitter.cb__CompUnit(self) };
    method node_name() { 'CompUnit' };
    method field_names() { ['statements','filename'] };
    method field_values() { [$.statements,$.filename] };
    method irx1_describe() {
      'CompUnit('~$.statements.irx1_describe~','~$.filename.irx1_describe~')'
    };
  };
  class Block is Base {
    has $.match;
    has $.statements;
    has $.notes;
    
    method newp($match,$statements) { self.new('match', $match, 'statements', $statements) };
    method callback($emitter) { $emitter.cb__Block(self) };
    method node_name() { 'Block' };
    method field_names() { ['statements'] };
    method field_values() { [$.statements] };
    method irx1_describe() {
      'Block('~$.statements.irx1_describe~')'
    };
  };
  class Use is Base {
    has $.match;
    has $.kind;
    has $.module_name;
    has $.expr;
    has $.notes;
    
    method newp($match,$kind,$module_name,$expr) { self.new('match', $match, 'kind', $kind, 'module_name', $module_name, 'expr', $expr) };
    method callback($emitter) { $emitter.cb__Use(self) };
    method node_name() { 'Use' };
    method field_names() { ['kind','module_name','expr'] };
    method field_values() { [$.kind,$.module_name,$.expr] };
    method irx1_describe() {
      'Use('~$.kind.irx1_describe~','~$.module_name.irx1_describe~','~$.expr.irx1_describe~')'
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
    has $.notes;
    
    method newp($match,$scope,$plurality,$kind,$name,$traits,$block) { self.new('match', $match, 'scope', $scope, 'plurality', $plurality, 'kind', $kind, 'name', $name, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__PackageDecl(self) };
    method node_name() { 'PackageDecl' };
    method field_names() { ['scope','plurality','kind','name','traits','block'] };
    method field_values() { [$.scope,$.plurality,$.kind,$.name,$.traits,$.block] };
    method irx1_describe() {
      'PackageDecl('~$.scope.irx1_describe~','~$.plurality.irx1_describe~','~$.kind.irx1_describe~','~$.name.irx1_describe~','~$.traits.irx1_describe~','~$.block.irx1_describe~')'
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
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block,$sigil,$postcircumfix) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block, 'sigil', $sigil, 'postcircumfix', $postcircumfix) };
    method callback($emitter) { $emitter.cb__MethodDecl(self) };
    method node_name() { 'MethodDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block','sigil','postcircumfix'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block,$.sigil,$.postcircumfix] };
    method irx1_describe() {
      'MethodDecl('~$.scope.irx1_describe~','~$.typenames.irx1_describe~','~$.plurality.irx1_describe~','~$.name.irx1_describe~','~$.multisig.irx1_describe~','~$.traits.irx1_describe~','~$.block.irx1_describe~','~$.sigil.irx1_describe~','~$.postcircumfix.irx1_describe~')'
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
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__SubDecl(self) };
    method node_name() { 'SubDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
    method irx1_describe() {
      'SubDecl('~$.scope.irx1_describe~','~$.typenames.irx1_describe~','~$.plurality.irx1_describe~','~$.name.irx1_describe~','~$.multisig.irx1_describe~','~$.traits.irx1_describe~','~$.block.irx1_describe~')'
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
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter) { $emitter.cb__MacroDecl(self) };
    method node_name() { 'MacroDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
    method irx1_describe() {
      'MacroDecl('~$.scope.irx1_describe~','~$.typenames.irx1_describe~','~$.plurality.irx1_describe~','~$.name.irx1_describe~','~$.multisig.irx1_describe~','~$.traits.irx1_describe~','~$.block.irx1_describe~')'
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
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$var,$postcircumfix,$traits,$default_op,$default_expr) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'var', $var, 'postcircumfix', $postcircumfix, 'traits', $traits, 'default_op', $default_op, 'default_expr', $default_expr) };
    method callback($emitter) { $emitter.cb__VarDecl(self) };
    method node_name() { 'VarDecl' };
    method field_names() { ['scope','typenames','plurality','var','postcircumfix','traits','default_op','default_expr'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.var,$.postcircumfix,$.traits,$.default_op,$.default_expr] };
    method irx1_describe() {
      'VarDecl('~$.scope.irx1_describe~','~$.typenames.irx1_describe~','~$.plurality.irx1_describe~','~$.var.irx1_describe~','~$.postcircumfix.irx1_describe~','~$.traits.irx1_describe~','~$.default_op.irx1_describe~','~$.default_expr.irx1_describe~')'
    };
  };
  class Var is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.notes;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter) { $emitter.cb__Var(self) };
    method node_name() { 'Var' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
    method irx1_describe() {
      'Var('~$.sigil.irx1_describe~','~$.twigil.irx1_describe~','~$.name.irx1_describe~')'
    };
  };
  class Trait is Base {
    has $.match;
    has $.verb;
    has $.expr;
    has $.notes;
    
    method newp($match,$verb,$expr) { self.new('match', $match, 'verb', $verb, 'expr', $expr) };
    method callback($emitter) { $emitter.cb__Trait(self) };
    method node_name() { 'Trait' };
    method field_names() { ['verb','expr'] };
    method field_values() { [$.verb,$.expr] };
    method irx1_describe() {
      'Trait('~$.verb.irx1_describe~','~$.expr.irx1_describe~')'
    };
  };
  class ClosureTrait is Base {
    has $.match;
    has $.kind;
    has $.block;
    has $.notes;
    
    method newp($match,$kind,$block) { self.new('match', $match, 'kind', $kind, 'block', $block) };
    method callback($emitter) { $emitter.cb__ClosureTrait(self) };
    method node_name() { 'ClosureTrait' };
    method field_names() { ['kind','block'] };
    method field_values() { [$.kind,$.block] };
    method irx1_describe() {
      'ClosureTrait('~$.kind.irx1_describe~','~$.block.irx1_describe~')'
    };
  };
  class ModuleName is Base {
    has $.match;
    has $.name;
    has $.pairs;
    has $.notes;
    
    method newp($match,$name,$pairs) { self.new('match', $match, 'name', $name, 'pairs', $pairs) };
    method callback($emitter) { $emitter.cb__ModuleName(self) };
    method node_name() { 'ModuleName' };
    method field_names() { ['name','pairs'] };
    method field_values() { [$.name,$.pairs] };
    method irx1_describe() {
      'ModuleName('~$.name.irx1_describe~','~$.pairs.irx1_describe~')'
    };
  };
  class PathName is Base {
    has $.match;
    has $.path;
    has $.notes;
    
    method newp($match,$path) { self.new('match', $match, 'path', $path) };
    method callback($emitter) { $emitter.cb__PathName(self) };
    method node_name() { 'PathName' };
    method field_names() { ['path'] };
    method field_values() { [$.path] };
    method irx1_describe() {
      'PathName('~$.path.irx1_describe~')'
    };
  };
  class SubName is Base {
    has $.match;
    has $.category;
    has $.pairs;
    has $.desigilname;
    has $.signature;
    has $.notes;
    
    method newp($match,$category,$pairs,$desigilname,$signature) { self.new('match', $match, 'category', $category, 'pairs', $pairs, 'desigilname', $desigilname, 'signature', $signature) };
    method callback($emitter) { $emitter.cb__SubName(self) };
    method node_name() { 'SubName' };
    method field_names() { ['category','pairs','desigilname','signature'] };
    method field_values() { [$.category,$.pairs,$.desigilname,$.signature] };
    method irx1_describe() {
      'SubName('~$.category.irx1_describe~','~$.pairs.irx1_describe~','~$.desigilname.irx1_describe~','~$.signature.irx1_describe~')'
    };
  };
  class ShapedParamName is Base {
    has $.match;
    has $.ident;
    has $.postcircumfix;
    has $.notes;
    
    method newp($match,$ident,$postcircumfix) { self.new('match', $match, 'ident', $ident, 'postcircumfix', $postcircumfix) };
    method callback($emitter) { $emitter.cb__ShapedParamName(self) };
    method node_name() { 'ShapedParamName' };
    method field_names() { ['ident','postcircumfix'] };
    method field_values() { [$.ident,$.postcircumfix] };
    method irx1_describe() {
      'ShapedParamName('~$.ident.irx1_describe~','~$.postcircumfix.irx1_describe~')'
    };
  };
  class Call is Base {
    has $.match;
    has $.invocant;
    has $.method;
    has $.capture;
    has $.notes;
    
    method newp($match,$invocant,$method,$capture) { self.new('match', $match, 'invocant', $invocant, 'method', $method, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Call(self) };
    method node_name() { 'Call' };
    method field_names() { ['invocant','method','capture'] };
    method field_values() { [$.invocant,$.method,$.capture] };
    method irx1_describe() {
      'Call('~$.invocant.irx1_describe~','~$.method.irx1_describe~','~$.capture.irx1_describe~')'
    };
  };
  class Apply is Base {
    has $.match;
    has $.function;
    has $.capture;
    has $.notes;
    
    method newp($match,$function,$capture) { self.new('match', $match, 'function', $function, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Apply(self) };
    method node_name() { 'Apply' };
    method field_names() { ['function','capture'] };
    method field_values() { [$.function,$.capture] };
    method irx1_describe() {
      'Apply('~$.function.irx1_describe~','~$.capture.irx1_describe~')'
    };
  };
  class Hyper is Base {
    has $.match;
    has $.operator;
    has $.capture;
    has $.notes;
    
    method newp($match,$operator,$capture) { self.new('match', $match, 'operator', $operator, 'capture', $capture) };
    method callback($emitter) { $emitter.cb__Hyper(self) };
    method node_name() { 'Hyper' };
    method field_names() { ['operator','capture'] };
    method field_values() { [$.operator,$.capture] };
    method irx1_describe() {
      'Hyper('~$.operator.irx1_describe~','~$.capture.irx1_describe~')'
    };
  };
  class Capture is Base {
    has $.match;
    has $.arguments;
    has $.notes;
    
    method newp($match,$arguments) { self.new('match', $match, 'arguments', $arguments) };
    method callback($emitter) { $emitter.cb__Capture(self) };
    method node_name() { 'Capture' };
    method field_names() { ['arguments'] };
    method field_values() { [$.arguments] };
    method irx1_describe() {
      'Capture('~$.arguments.irx1_describe~')'
    };
  };
  class MultiSig is Base {
    has $.match;
    has $.signatures;
    has $.notes;
    
    method newp($match,$signatures) { self.new('match', $match, 'signatures', $signatures) };
    method callback($emitter) { $emitter.cb__MultiSig(self) };
    method node_name() { 'MultiSig' };
    method field_names() { ['signatures'] };
    method field_values() { [$.signatures] };
    method irx1_describe() {
      'MultiSig('~$.signatures.irx1_describe~')'
    };
  };
  class Signature is Base {
    has $.match;
    has $.parameters;
    has $.return_type;
    has $.notes;
    
    method newp($match,$parameters,$return_type) { self.new('match', $match, 'parameters', $parameters, 'return_type', $return_type) };
    method callback($emitter) { $emitter.cb__Signature(self) };
    method node_name() { 'Signature' };
    method field_names() { ['parameters','return_type'] };
    method field_values() { [$.parameters,$.return_type] };
    method irx1_describe() {
      'Signature('~$.parameters.irx1_describe~','~$.return_type.irx1_describe~')'
    };
  };
  class Parameter is Base {
    has $.match;
    has $.type_constraints;
    has $.quant;
    has $.param_var;
    has $.ident;
    has $.traits;
    has $.post_constraints;
    has $.default_expr;
    has $.notes;
    
    method newp($match,$type_constraints,$quant,$param_var,$ident,$traits,$post_constraints,$default_expr) { self.new('match', $match, 'type_constraints', $type_constraints, 'quant', $quant, 'param_var', $param_var, 'ident', $ident, 'traits', $traits, 'post_constraints', $post_constraints, 'default_expr', $default_expr) };
    method callback($emitter) { $emitter.cb__Parameter(self) };
    method node_name() { 'Parameter' };
    method field_names() { ['type_constraints','quant','param_var','ident','traits','post_constraints','default_expr'] };
    method field_values() { [$.type_constraints,$.quant,$.param_var,$.ident,$.traits,$.post_constraints,$.default_expr] };
    method irx1_describe() {
      'Parameter('~$.type_constraints.irx1_describe~','~$.quant.irx1_describe~','~$.param_var.irx1_describe~','~$.ident.irx1_describe~','~$.traits.irx1_describe~','~$.post_constraints.irx1_describe~','~$.default_expr.irx1_describe~')'
    };
  };
  class TypeConstraint is Base {
    has $.match;
    has $.value;
    has $.where_expr;
    has $.notes;
    
    method newp($match,$value,$where_expr) { self.new('match', $match, 'value', $value, 'where_expr', $where_expr) };
    method callback($emitter) { $emitter.cb__TypeConstraint(self) };
    method node_name() { 'TypeConstraint' };
    method field_names() { ['value','where_expr'] };
    method field_values() { [$.value,$.where_expr] };
    method irx1_describe() {
      'TypeConstraint('~$.value.irx1_describe~','~$.where_expr.irx1_describe~')'
    };
  };
  class PostConstraint is Base {
    has $.match;
    has $.multisig;
    has $.where_expr;
    has $.notes;
    
    method newp($match,$multisig,$where_expr) { self.new('match', $match, 'multisig', $multisig, 'where_expr', $where_expr) };
    method callback($emitter) { $emitter.cb__PostConstraint(self) };
    method node_name() { 'PostConstraint' };
    method field_names() { ['multisig','where_expr'] };
    method field_values() { [$.multisig,$.where_expr] };
    method irx1_describe() {
      'PostConstraint('~$.multisig.irx1_describe~','~$.where_expr.irx1_describe~')'
    };
  };
  class ParamVar is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.notes;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter) { $emitter.cb__ParamVar(self) };
    method node_name() { 'ParamVar' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
    method irx1_describe() {
      'ParamVar('~$.sigil.irx1_describe~','~$.twigil.irx1_describe~','~$.name.irx1_describe~')'
    };
  };
  class Undef is Base {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter) { $emitter.cb__Undef(self) };
    method node_name() { 'Undef' };
    method field_names() { [] };
    method field_values() { [] };
    method irx1_describe() {
      'Undef('~')'
    };
  };
  class NumInt is Base {
    has $.match;
    has $.text;
    has $.base;
    has $.notes;
    
    method newp($match,$text,$base) { self.new('match', $match, 'text', $text, 'base', $base) };
    method callback($emitter) { $emitter.cb__NumInt(self) };
    method node_name() { 'NumInt' };
    method field_names() { ['text','base'] };
    method field_values() { [$.text,$.base] };
    method irx1_describe() {
      'NumInt('~$.text.irx1_describe~','~$.base.irx1_describe~')'
    };
  };
  class NumDec is Base {
    has $.match;
    has $.intpart;
    has $.fracpart;
    has $.exp;
    has $.notes;
    
    method newp($match,$intpart,$fracpart,$exp) { self.new('match', $match, 'intpart', $intpart, 'fracpart', $fracpart, 'exp', $exp) };
    method callback($emitter) { $emitter.cb__NumDec(self) };
    method node_name() { 'NumDec' };
    method field_names() { ['intpart','fracpart','exp'] };
    method field_values() { [$.intpart,$.fracpart,$.exp] };
    method irx1_describe() {
      'NumDec('~$.intpart.irx1_describe~','~$.fracpart.irx1_describe~','~$.exp.irx1_describe~')'
    };
  };
  class NumRad is Base {
    has $.match;
    has $.radix;
    has $.intpart;
    has $.fracpart;
    has $.base;
    has $.exp;
    has $.notes;
    
    method newp($match,$radix,$intpart,$fracpart,$base,$exp) { self.new('match', $match, 'radix', $radix, 'intpart', $intpart, 'fracpart', $fracpart, 'base', $base, 'exp', $exp) };
    method callback($emitter) { $emitter.cb__NumRad(self) };
    method node_name() { 'NumRad' };
    method field_names() { ['radix','intpart','fracpart','base','exp'] };
    method field_values() { [$.radix,$.intpart,$.fracpart,$.base,$.exp] };
    method irx1_describe() {
      'NumRad('~$.radix.irx1_describe~','~$.intpart.irx1_describe~','~$.fracpart.irx1_describe~','~$.base.irx1_describe~','~$.exp.irx1_describe~')'
    };
  };
  class Array is Base {
    has $.match;
    has $.array;
    has $.notes;
    
    method newp($match,$array) { self.new('match', $match, 'array', $array) };
    method callback($emitter) { $emitter.cb__Array(self) };
    method node_name() { 'Array' };
    method field_names() { ['array'] };
    method field_values() { [$.array] };
    method irx1_describe() {
      'Array('~$.array.irx1_describe~')'
    };
  };
  class Hash is Base {
    has $.match;
    has $.hash;
    has $.notes;
    
    method newp($match,$hash) { self.new('match', $match, 'hash', $hash) };
    method callback($emitter) { $emitter.cb__Hash(self) };
    method node_name() { 'Hash' };
    method field_names() { ['hash'] };
    method field_values() { [$.hash] };
    method irx1_describe() {
      'Hash('~$.hash.irx1_describe~')'
    };
  };
  class Pair is Base {
    has $.match;
    has $.key;
    has $.value;
    has $.notes;
    
    method newp($match,$key,$value) { self.new('match', $match, 'key', $key, 'value', $value) };
    method callback($emitter) { $emitter.cb__Pair(self) };
    method node_name() { 'Pair' };
    method field_names() { ['key','value'] };
    method field_values() { [$.key,$.value] };
    method irx1_describe() {
      'Pair('~$.key.irx1_describe~','~$.value.irx1_describe~')'
    };
  };
  class Type is Base {
    has $.match;
    has $.typename;
    has $.notes;
    
    method newp($match,$typename) { self.new('match', $match, 'typename', $typename) };
    method callback($emitter) { $emitter.cb__Type(self) };
    method node_name() { 'Type' };
    method field_names() { ['typename'] };
    method field_values() { [$.typename] };
    method irx1_describe() {
      'Type('~$.typename.irx1_describe~')'
    };
  };
  class Rx is Base {
    has $.match;
    has $.pat;
    has $.notes;
    
    method newp($match,$pat) { self.new('match', $match, 'pat', $pat) };
    method callback($emitter) { $emitter.cb__Rx(self) };
    method node_name() { 'Rx' };
    method field_names() { ['pat'] };
    method field_values() { [$.pat] };
    method irx1_describe() {
      'Rx('~$.pat.irx1_describe~')'
    };
  };
  class Buf is Base {
    has $.match;
    has $.buf;
    has $.notes;
    
    method newp($match,$buf) { self.new('match', $match, 'buf', $buf) };
    method callback($emitter) { $emitter.cb__Buf(self) };
    method node_name() { 'Buf' };
    method field_names() { ['buf'] };
    method field_values() { [$.buf] };
    method irx1_describe() {
      'Buf('~$.buf.irx1_describe~')'
    };
  };
  class For is Base {
    has $.match;
    has $.expr;
    has $.block;
    has $.notes;
    
    method newp($match,$expr,$block) { self.new('match', $match, 'expr', $expr, 'block', $block) };
    method callback($emitter) { $emitter.cb__For(self) };
    method node_name() { 'For' };
    method field_names() { ['expr','block'] };
    method field_values() { [$.expr,$.block] };
    method irx1_describe() {
      'For('~$.expr.irx1_describe~','~$.block.irx1_describe~')'
    };
  };
  class Cond is Base {
    has $.match;
    has $.clauses;
    has $.default;
    has $.invert_first_test;
    has $.notes;
    
    method newp($match,$clauses,$default,$invert_first_test) { self.new('match', $match, 'clauses', $clauses, 'default', $default, 'invert_first_test', $invert_first_test) };
    method callback($emitter) { $emitter.cb__Cond(self) };
    method node_name() { 'Cond' };
    method field_names() { ['clauses','default','invert_first_test'] };
    method field_values() { [$.clauses,$.default,$.invert_first_test] };
    method irx1_describe() {
      'Cond('~$.clauses.irx1_describe~','~$.default.irx1_describe~','~$.invert_first_test.irx1_describe~')'
    };
  };
  class Loop is Base {
    has $.match;
    has $.pretest;
    has $.block;
    has $.posttest;
    has $.label;
    has $.notes;
    
    method newp($match,$pretest,$block,$posttest,$label) { self.new('match', $match, 'pretest', $pretest, 'block', $block, 'posttest', $posttest, 'label', $label) };
    method callback($emitter) { $emitter.cb__Loop(self) };
    method node_name() { 'Loop' };
    method field_names() { ['pretest','block','posttest','label'] };
    method field_values() { [$.pretest,$.block,$.posttest,$.label] };
    method irx1_describe() {
      'Loop('~$.pretest.irx1_describe~','~$.block.irx1_describe~','~$.posttest.irx1_describe~','~$.label.irx1_describe~')'
    };
  };
}
